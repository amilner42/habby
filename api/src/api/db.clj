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

(defonce habby_db (mg/get-db connection "habby"))

(defn add-habit
  "Add a habit to the database and returns that habit including the ID.
  Will create an ID if the habit passed doesn't have an ID. Will set `suspended` to false."
  [{:keys [db habit] :or {db habby_db}}]
  (let [ final_habit (as-> habit habit
                          (if (contains? habit :_id) habit (assoc habit :_id (ObjectId.)))
                          (assoc habit :suspended false))]
    (mc/insert-and-return db (:habits collection-names) final_habit)))

(defn delete-habit
  "Deletes a habit from the database, returns true if the habit was deleted."
  [{:keys [db habit_id] :or {db habby_db}}]
  (= 1 (.getN (mc/remove-by-id db (:habits collection-names) (ObjectId. habit_id)))))

(defn set-suspend-habit
  "Set the `suspended` for a habit, returns true if the update was performed on the habit."
  [{:keys [db habit_id suspended] :or {db habby_db}}]
  (= 1 (.getN (mc/update db (:habits collection-names) {:_id (ObjectId. habit_id)} {$set {:suspended suspended}}))))

(defn get-habits
  "Retrieves all habits sync from the database as clojure maps."
  [{:keys [db] :or {db habby_db}}]
  (mc/find-maps db (:habits collection-names)))

(defn get-habit-data
  "Gets habit data from the db, optionally after a specific date or for a specific habit."
  [{:keys [db after_date for_habit] :or {db habby_db}}]
  (as-> {} find-query-filter
        (if (nil? for_habit) find-query-filter (assoc find-query-filter :habit_id (ObjectId. for_habit)))
        (if (nil? after_date) find-query-filter (assoc find-query-filter :date {$gte (date-from-y-m-d-map after_date)}))
        (mc/find-maps db (:habit_data collection-names) find-query-filter)))

(defn set-habit-data
  "Set the `amount` for a habit on a specfic day."
  [{:keys [db habit_id amount date-time] :or {db habby_db}}]
  (mc/find-and-modify db
                      (:habit_data collection-names)
                      {:date date-time, :habit_id (ObjectId. habit_id)}
                      {$set {:amount amount}
                       $setOnInsert {:date date-time, :habit_id (ObjectId. habit_id), :_id (ObjectId.)}}
                      {:upsert true, :return-new true}))

(defn get-frequency-stats
  "Input: a list of habit IDs
  Output: A list of habit_frequency_stats (one for each habit ID provided)"
  [{:keys [db habit_ids] :or {db habby_db}}]
  (map (fn [habit]
         (let [sorted_habit_data (sort-by :date (get-habit-data {:db db :for_habit (str (:_id habit))}))]
           (if (empty? sorted_habit_data)
             nil  ; Return nil if no habit data exists for the habit being checked
             (let [habit_type (:type_name habit)
                   freq (if (= habit_type "good_habit") (:target_frequency habit) (:threshold_frequency habit))
                   freq_type (:type_name freq)
                   compare_fn (if (= habit_type "good_habit") >= <=)]
                  (loop
                    [total_fragments 0
                     successful_fragments 0
                     total_done 0
                     current_fragment_streak 0
                     best_fragment_streak 0
                     remaining_habit_data sorted_habit_data
                     fragment_start_date (if (= freq_type "total_week_frequency")
                                           ; Start calendar week fragment on the Monday before the first habit record
                                           (t/minus (:date (first sorted_habit_data))
                                                    (t/days (- (t/day-of-week (:date (first sorted_habit_data)))
                                                               1)))
                                           ; Start fragment at the date of the first habit record
                                           (:date (first sorted_habit_data)))]
                    (let
                      [fragment_end_date (condp = freq_type
                                           "specific_day_of_week_frequency" fragment_start_date
                                           "total_week_frequency" (t/plus fragment_start_date
                                                                          (t/days 6))
                                           "every_x_days_frequency" (t/plus fragment_start_date
                                                                            (t/days (dec (:days freq)))))
                       fragment_goal (condp = freq_type
                                       "specific_day_of_week_frequency" ((condp = (t/day-of-week fragment_start_date)
                                                                           1 :monday 2 :tuesday 3 :wednesday 4 :thursday
                                                                           5 :friday 6 :saturday 7 :sunday)
                                                                         freq)
                                       "total_week_frequency" (:week freq)
                                       "every_x_days_frequency" (:times freq))
                       habit_data_partition (group-by #(if (or (are-datetimes-same-date (:date %) fragment_start_date)
                                                               (are-datetimes-same-date (:date %) fragment_end_date)
                                                               (and (t/after? (:date %) fragment_start_date)
                                                                    (t/before? (:date %) fragment_end_date)))
                                                         :fragment_data
                                                         :other_data)
                                                      remaining_habit_data)
                       total_done_during_fragment (reduce #(+ %1 (:amount %2)) 0 (:fragment_data habit_data_partition))]
                      (if (or (t/after? fragment_end_date (t/today-at 0 0))
                              (t/equal? fragment_end_date (t/today-at 0 0)))
                        ; We've reached the current fragment the user is on, return now.
                        ; We don't include the current fragment in success stats but we do increase total_done.
                        {:habit_id (str (:_id habit))
                         :total_fragments total_fragments
                         :successful_fragments successful_fragments
                         :total_done (+ total_done total_done_during_fragment)
                         :current_fragment_streak current_fragment_streak
                         :best_fragment_streak best_fragment_streak
                         :current_fragment_total total_done_during_fragment
                         :current_fragment_goal fragment_goal
                         :current_fragment_days_left (inc (t/in-days (t/interval (t/today-at 0 0) fragment_end_date)))}
                        ; Track the current fragment
                        (let [fragment_is_successful (compare_fn total_done_during_fragment fragment_goal)
                              new_current_fragment_streak (if fragment_is_successful (inc current_fragment_streak) 0)]
                          (recur (inc total_fragments)
                                 (if fragment_is_successful (inc successful_fragments) successful_fragments)
                                 (+ total_done total_done_during_fragment)
                                 new_current_fragment_streak
                                 (if (> new_current_fragment_streak best_fragment_streak)
                                   new_current_fragment_streak
                                   best_fragment_streak)
                                 (:other_data habit_data_partition)
                                 (t/plus fragment_start_date
                                         (t/days (condp = freq_type
                                                   "specific_day_of_week_frequency" 1
                                                   "total_week_frequency" 7
                                                   "every_x_days_frequency" (:days freq)))))))))))))
       (if (nil? habit_ids)
         (get-habits {:db db})
         (map (fn [habit_id] (mc/find-map-by-id db
                                                (:habits collection-names)
                                                (ObjectId. habit_id)))
              habit_ids))))
