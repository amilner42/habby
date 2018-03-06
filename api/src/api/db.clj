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

(defn get-frequency-stats
  "Input: a list of habit IDs
  Output: A list of habit_frequency_stats (one for each habit ID provided)"
  [habit_ids]
  (map (fn [habit]
         (let [sorted_habit_data (sort-by :date (get-habit-data nil (str (:_id habit))))]
           (if (empty? sorted_habit_data)
             nil  ; Return nil if no habit data exists for the habit being checked
             (let [habit_type (:type_name habit)
                   freq (if (= habit_type "good_habit")
                          (:target_frequency habit)
                          (:threshold_frequency habit))
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
                                           ; Start calendar week fragment on the Monday after the first habit record
                                           (t/plus (:date (first sorted_habit_data))
                                                   (t/days (- 7 (t/day-of-week (:date (first sorted_habit_data))))))
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
                                                                           1 :monday
                                                                           2 :tuesday
                                                                           3 :wednesday
                                                                           4 :thursday
                                                                           5 :friday
                                                                           6 :saturday
                                                                           7 :sunday)
                                                                         freq)
                                       "total_week_frequency" (:week freq)
                                       "every_x_days_frequency" (:times freq))]
                      (if (or (t/after? fragment_end_date (t/today-at 0 0))
                              (t/equal? fragment_end_date (t/today-at 0 0)))
                        ; Don't track a fragment that hasn't ended yet, return now
                        {:habit_id (str (:_id habit))
                         :total_fragments total_fragments
                         :successful_fragments successful_fragments
                         :total_done total_done
                         :fragment_streak best_fragment_streak}
                        ; Track the current fragment
                        (let [habit_data_partition (group-by #(if (or (are-datetimes-same-date (:date %)
                                                                                               fragment_start_date)
                                                                      (are-datetimes-same-date (:date %)
                                                                                               fragment_end_date)
                                                                      (and (t/after? (:date %) fragment_start_date)
                                                                           (t/before? (:date %) fragment_end_date)))
                                                                :habit_data_within_fragment
                                                                :habit_data_outside_fragment)
                                                             remaining_habit_data)
                              total_done_during_fragment (reduce #(+ %1 (:amount %2))
                                                                 0
                                                                 (:habit_data_within_fragment habit_data_partition))
                              fragment_is_successful (compare_fn total_done_during_fragment fragment_goal)
                              new_current_fragment_streak (if fragment_is_successful (inc current_fragment_streak) 0)]
                          (recur (inc total_fragments)
                                 (if fragment_is_successful (inc successful_fragments) successful_fragments)
                                 (+ total_done total_done_during_fragment)
                                 new_current_fragment_streak
                                 (if (> new_current_fragment_streak best_fragment_streak)
                                   new_current_fragment_streak
                                   best_fragment_streak)
                                 (:habit_data_outside_fragment habit_data_partition)
                                 (t/plus fragment_start_date
                                         (t/days (condp = freq_type
                                                   "specific_day_of_week_frequency" 1
                                                   "total_week_frequency" 7
                                                   "every_x_days_frequency" (:days freq)))))))))))))
       (if (nil? habit_ids)
         (get-habits)
         (map (fn [habit_id] (mc/find-map-by-id db
                                                (:habits collection-names)
                                                (ObjectId. habit_id)))
              habit_ids))))
