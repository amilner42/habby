(ns api.db
  "Interaction with the database happens in this namespace"
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.joda-time]
            [api.util :refer :all]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId org.joda.time.DateTimeZone))

(DateTimeZone/setDefault DateTimeZone/UTC)

(def collection-names
  "The names of all the collections in the database."
  {:habits "habits"
   :habit_data "habit_data"})

(defonce connection (mg/connect))

(defonce db (mg/get-db connection "habby"))

(defn add-habit
  "Add a habit to the database and returns that habit including the ID.
  Will create an ID if the habit passed doesn't have an ID. Will set `suspended` to false."
  [habit]
  (let [ final_habit (as-> habit habit
                          (if (contains? habit :_id) habit (assoc habit :_id (ObjectId.)))
                          (assoc habit :suspended false))]
    (mc/insert-and-return db (:habits collection-names) final_habit)))

(defn delete-habit
  "Deletes a habit from the database, returns true if the habit was deleted."
  [habit_id]
  (= 1 (.getN (mc/remove-by-id db (:habits collection-names) (ObjectId. habit_id)))))

(defn set-suspend-habit
  "Set the `suspended` for a habit, returns true if the update was performed on the habit."
  [habit_id suspended]
  (= 1 (.getN (mc/update db (:habits collection-names) {:_id (ObjectId. habit_id)} {$set {:suspended suspended}}))))

(defn get-habits
  "Retrieves all habits sync from the database as clojure maps."
  []
  (mc/find-maps db (:habits collection-names)))

(defn get-habit-data
  "Gets habit data from the db, optionally after a specific date or for a specific habit."
  [after_date for_habit]
  (as-> {} find-query-filter
        (if (nil? for_habit) find-query-filter (assoc find-query-filter :habit_id (ObjectId. for_habit)))
        (if (nil? after_date) find-query-filter (assoc find-query-filter :date {$gte (date-from-y-m-d-map after_date)}))
        (mc/find-maps db (:habit_data collection-names) find-query-filter)))

(defn set-habit-data
  "Set the `amount` for a habit on a specfic day."
  [habit_id amount date-time]
  (mc/find-and-modify
             db
             (:habit_data collection-names)
             {:date date-time, :habit_id (ObjectId. habit_id)}
             {$set {:amount amount}
              $setOnInsert {:date date-time, :habit_id (ObjectId. habit_id), :_id (ObjectId.)}}
             {:upsert true, :return-new true}))

(defn get-every-x-days-stats
  "Input: a list of habit data (assumed sorted by date), the compare function
      (>= for good habits, <= for bad habits), <times> and <days> where the
      habit goal is <times> times per <days> days
  Output: a percentage of successful blocks of days"
  [habit_data compare_fn times days]
  (loop [start_date (:date (first habit_data))  ; start date of current block
         date_index 0  ; which index of habit_data we have checked up to
         num_successes 0  ; number of successful blocks
         block_count 0]  ; total number of blocks
    (if (nil? start_date)
      0
      (if (or (t/after? start_date (t/today-at 0 0)) (t/equal? start_date (t/today-at 0 0)))
          ; If we've reached a segment that starts tomorrow or later, return the
          ;  final percentage
          (ratio-to-percentage num_successes block_count)
          ; Else, track the num_successes in the current block
          (let [block_info
                (reduce (fn [acc date]
                          (if (or (>= (:i acc) (count habit_data))
                                  (not (are-datetimes-same-date date (:date (nth habit_data (:i acc))))))
                            ; No habit record exists for this date, so block_count it as 0 and leave
                            ;  :total_done alone. :i stays the same too since we didn't use it.
                            acc
                            ; Increment :i by 1, and increase :total_done by the amount done
                            (assoc (assoc acc :i (inc (:i acc)))
                                   :total_done (+ (:total_done acc)
                                                  (:amount (nth habit_data (:i acc)))))))
                        {:i date_index, :total_done 0}
                        (map (fn [i] (t/plus start_date (t/days i)))
                             (range days)))
                total_done (:total_done block_info)
                i (:i block_info)]
            (recur (t/plus start_date (t/days days))  ; update start_date
                   i  ; update date_index
                   (if (compare_fn total_done times)  ; update num_successes
                     (inc num_successes)
                     num_successes)
                   (inc block_count)))))))  ; update block_count

(defn get-frequency-stats
  "Input: a list of habit IDs
  Output: A list of percentages (one for each habit ID)"
  [habit_ids]
  (map (fn [habit] (let [habit_type (:type_name habit)
                         freq (if (= habit_type "good_habit")
                                (:target_frequency habit)
                                (:threshold_frequency habit))
                         freq_type (:type_name freq)
                         habit_data (get-habit-data nil (str (:_id habit)))]
                     (cond
                       (= freq_type "every_x_days_frequency")
                       (get-every-x-days-stats habit_data
                                               (if (= habit_type "good_habit") >= <=)
                                               (:times freq)
                                               (:days freq)))))
       (if (nil? habit_ids)
         (get-habits)
         (map (fn [habit_id] (mc/find-map-by-id db
                                                (:habits collection-names)
                                                (ObjectId. habit_id)))
              habit_ids))))
