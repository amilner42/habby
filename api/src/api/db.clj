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

(defn get-every-x-days-frequency-stats
  "Input: a list of habit data (assumed sorted by date), the compare function
      (>= for good habits, <= for bad habits), <times> and <days> where the
      habit goal is <times> times per <days> days, the habit ID
  Output: a map:
      {`:habit_id`: the habit's ID
       `:total_fragments`: the total number of fragments that have passed
       `:successful_fragments`: the number of fragments where the habit goal was achieved
       `:total_done`: total amount of the habit done across all fragments
       `:fragment_streak`: best streak of successful fragments}"
  [habit_data compare_fn times days habit_id]
  (loop [start_date (:date (first habit_data))  ; start date of current block
         date_index 0  ; which index of habit_data we have checked up to
         block_count 0  ; total number of blocks
         num_successes 0  ; number of successful blocks
         total_done 0  ; total amount done of the habit
         current_streak 0  ; current streak of successful blocks
         best_streak 0]  ; best ever streak of successful blocks
    (if (nil? start_date)
      ; No habit data exists, so just treat everything as 0 since the user may not have started the habit yet
      {:habit_id habit_id
       :total_fragments 0
       :successful_fragments 0
       :total_done 0
       :fragment_streak 0}
      ; Habit data exists, we can return a more meaningful statistics map
      (if (or (t/after? start_date (t/today-at 0 0)) (t/equal? start_date (t/today-at 0 0)))
          ; If we've reached a segment that starts tomorrow or later, return the
          ;  final map
          {:habit_id habit_id
           :total_fragments block_count
           :successful_fragments num_successes
           :total_done total_done
           :fragment_streak best_streak}
          ; Else, update the stats with the current block
          (let [block_info
                (reduce (fn [acc date]
                          (if (or (>= (:i acc) (count habit_data))
                                  (not (are-datetimes-same-date date (:date (nth habit_data (:i acc))))))
                            ; No habit record exists for this date, so count it as 0 and leave
                            ;  :total_done alone. :i stays the same too since we didn't use it.
                            acc
                            ; Increment :i by 1, and increase :total_done by the amount done
                            (assoc (assoc acc :i (inc (:i acc)))
                                   :total_done_in_block (+ (:total_done_in_block acc)
                                                           (:amount (nth habit_data (:i acc)))))))
                        {:i date_index,
                         :total_done_in_block 0}
                        (map (fn [i] (t/plus start_date (t/days i)))
                             (range days)))
                total_done_in_block (:total_done_in_block block_info)
                i (:i block_info)
                is_block_successful (compare_fn total_done_in_block times)
                new_current_streak (if is_block_successful (inc current_streak) 0)]
            (recur (t/plus start_date (t/days days))  ; update start_date
                   i  ; update date_index
                   (inc block_count)  ; update block_count
                   (if is_block_successful  ; update num_successes
                     (inc num_successes)
                     num_successes)
                   (+ total_done total_done_in_block)  ; update total_done
                   new_current_streak  ; update current_streak
                   (if (> new_current_streak best_streak)  ; update best_streak
                     new_current_streak
                     best_streak)))))))

(defn day-of-week-int-to-keyword
  "Input:
      <day_int>: the Int representing a day of the week according to clj-time
  Output: a keyword representing the <day_int>-th day of the week"
  [day_int]
  (cond
    (= day_int 1) :monday
    (= day_int 2) :tuesday
    (= day_int 3) :wednesday
    (= day_int 4) :thursday
    (= day_int 5) :friday
    (= day_int 6) :saturday
    (= day_int 7) :sunday
    :else (throw (Exception. "day_int out of range, can't convert to day of week"))))

(defn get-specific-day-of-week-frequency-stats
  "Input:
      <habit_data>: habit day records sorted by date
      <compare_fn>: >= for good habits, <= for bad habits
      <week_goal_map>: a map of goal times per day of the week (i.e. a specific_day_of_week_frequency)
      <habit_id>: the habit's ID
  Output: a map:
      {`:habit_id`: the habit's ID
       `:total_fragments`: the total number of days the habit should have been done (i.e. mandatory days)
       `:successful_fragments`: the number of days where the habit goal was achieved
       `:total_done`: total amount of the habit done across all days (including optional days)
       `:fragment_streak`: best streak of successful mandatory days}"
  [habit_data compare_fn week_goal_map habit_id]
  (loop [total_fragments 0
         successful_fragments 0
         total_done 0
         current_streak 0
         best_streak 0
         remaining_habit_data habit_data  ; habit records left to check
         date_to_check (:date (first habit_data))]
    (if (or (t/after? date_to_check (t/today-at 0 0))
            (t/equal? date_to_check (t/today-at 0 0)))
      ; We've reached today or later, stop tracking and return final map
      {:habit_id habit_id
       :total_fragments total_fragments
       :successful_fragments successful_fragments
       :total_done total_done
       :fragment_streak best_streak}
      ; There's at least one more date to check habit data for
      (let [habit_record_to_check (first remaining_habit_data)
            habit_record_date (:date habit_record_to_check)  ; date of the next habit record to be checked
            habit_record_is_correct_date (and (not (nil? habit_record_to_check))
                                              (are-datetimes-same-date date_to_check habit_record_date))
            habit_amount (if habit_record_is_correct_date
                           (:amount habit_record_to_check)
                           0)  ; no habit record exists for the date we're checking so treat it as 0
            day_of_week (t/day-of-week date_to_check)  ; day of week (int) of the date we are checking
            goal_amount (week_goal_map (day-of-week-int-to-keyword day_of_week))
            successful_day (compare_fn habit_amount goal_amount)
            new_current_streak (if successful_day (inc current_streak) 0)]
        (recur (inc total_fragments)
               (if successful_day (inc successful_fragments) successful_fragments)
               (+ habit_amount total_done)
               new_current_streak
               (if (> new_current_streak best_streak) new_current_streak best_streak)
               (if habit_record_is_correct_date (rest remaining_habit_data) remaining_habit_data)
               (t/plus date_to_check (t/days 1)))))))

(defn get-total-week-frequency-stats
  "Input:
      <habit_data>: habit day records sorted by date
      <compare_fn>: >= for good habits, <= for bad habits
      <weekly_goal>: Goal amount for the week (minimum for good habits, maximum for bad habits)
      <habit_id>: the habit's ID
  Output: a map:
      {`:habit_id`: the habit's ID
       `:total_fragments`: the total number of days the habit should have been done (i.e. mandatory days)
       `:successful_fragments`: the number of days where the habit goal was achieved
       `:total_done`: total amount of the habit done across all days (including optional days)
       `:fragment_streak`: best streak of successful mandatory days}"
  [habit_data compare_fn weekly_goal habit_id]
  (loop [total_fragments 0
         successful_fragments 0
         total_done 0
         current_streak 0
         best_streak 0
         remaining_habit_data habit_data
         start_of_week_to_check (t/plus (:date (first habit_data))  ; calendar week should start at sunday
                                        (t/days (- 7 (t/day-of-week (:date (first habit_data))))))]
    (let [end_of_week_to_check (t/plus start_of_week_to_check (t/days 6))]
      (if (or (t/after? end_of_week_to_check (t/today-at 0 0))
              (t/equal? end_of_week_to_check (t/today-at 0 0)))
        ; We're done checking, now return. (Not tracking the current week since it hasn't ended)
        {:habit_id habit_id
         :total_fragments total_fragments
         :successful_fragments successful_fragments
         :total_done total_done
         :fragment_streak best_streak}
        ; Track the current week
        (let [habit_data_partition (group-by #(if (or (are-datetimes-same-date (:date %) start_of_week_to_check)
                                                      (are-datetimes-same-date (:date %) end_of_week_to_check)
                                                      (and (t/after? (:date %) start_of_week_to_check)
                                                           (t/before? (:date %) end_of_week_to_check)))
                                                :this_weeks_data
                                                :not_this_weeks_data)
                                             remaining_habit_data)
              habit_records_from_this_week (:this_weeks_data habit_data_partition)
              weekly_amount (reduce #(+ %1 (:amount %2)) 0 habit_records_from_this_week)
              successful_week (compare_fn weekly_amount weekly_goal)
              new_current_streak (if successful_week (inc current_streak) 0)]
          (recur (inc total_fragments)
                 (if successful_week (inc successful_fragments) successful_fragments)
                 (+ weekly_amount total_done)
                 new_current_streak
                 (if (> new_current_streak best_streak) new_current_streak best_streak)
                 (:not_this_weeks_data habit_data_partition)
                 (t/plus start_of_week_to_check (t/days 7))))))))

(defn get-frequency-stats
  "Input: a list of habit IDs
  Output: A list of habit_frequency_stats (one for each habit ID provided)"
  [habit_ids]
  (map (fn [habit] (let [habit_type (:type_name habit)
                         freq (if (= habit_type "good_habit")
                                (:target_frequency habit)
                                (:threshold_frequency habit))
                         freq_type (:type_name freq)
                         sorted_habit_data (sort-by :date (get-habit-data nil (str (:_id habit))))
                         compare_fn (if (= habit_type "good_habit") >= <=)]
                     (cond
                       (= freq_type "every_x_days_frequency")
                       (get-every-x-days-frequency-stats
                         sorted_habit_data
                         compare_fn
                         (:times freq)
                         (:days freq)
                         (:_id habit))
                       (= freq_type "specific_day_of_week_frequency")
                       (get-specific-day-of-week-frequency-stats sorted_habit_data
                                                                 compare_fn
                                                                 freq
                                                                 (:_id habit))
                       (= freq_type "total_week_frequency")
                       (get-total-week-frequency-stats sorted_habit_data
                                                       compare_fn
                                                       (:week freq)
                                                       (:_id habit)))))
       (if (nil? habit_ids)
         (get-habits)
         (map (fn [habit_id] (mc/find-map-by-id db
                                                (:habits collection-names)
                                                (ObjectId. habit_id)))
              habit_ids))))
