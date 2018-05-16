(ns api.freq-stats-util
  "A namespace for holding utilities related to calculation of performance statistics."
  (:require [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime, get-consecutive-datetimes,
                                 day-of-week-keyword, days-spanned-between-datetimes]]
            [api.habit-util :refer [get-frequency]]
            [clj-time.core :as t]))

(def default-frequency-stats {:total_fragments 0, :successful_fragments 0, :total_done 0,
                              :current_fragment_streak 0, :best_fragment_streak 0,
                              :current_fragment_total 0, :current_fragment_goal 0, :current_fragment_days_left 0})

(defn get-habit-goal-fragment-length
  "Returns the number of days within each habit goal fragment, given a goal frequency `freq`.
  A habit goal fragment is a fragment of time that we will treat as either a success or a failure for a habit."
  [freq]
  (condp = (:type_name freq)
         "specific_day_of_week_frequency" 1,
         "total_week_frequency" 7,
         "every_x_days_frequency" (:days freq)))

(defn get-habit-goal-amount-for-datetime
  "Returns the target/threshold amount for a particular DateTime, given the goal frequency."
  [dt freq]
  (condp = (:type_name freq)
         "specific_day_of_week_frequency" (-> dt
                                              day-of-week-keyword
                                              freq),
         "total_week_frequency" (:week freq),
         "every_x_days_frequency" (:times freq)))

(defn get-habit-start-date
  "Returns the `DateTime` on which we consider a habit to have started, given its data and goal.
  If `freq` is based on the calendar week (Monday to Sunday), returns the Monday of the first `habit_day_record`.
  Otherwise just returns the date of the first `habit_day_record`.
  Assumes `sorted-habit-data` is sorted increasingly by date and non-empty."
  [sorted-habit-data freq]
  (let [date-of-first-habit-day-record (:date (first sorted-habit-data))]
    (if (= (:type_name freq) "total_week_frequency")
      (first-monday-before-datetime date-of-first-habit-day-record)
      date-of-first-habit-day-record)))

(defn partition-datetimes-based-on-habit-goal
  "Returns a partition of consecutive DateTimes corresponding to habit goal fragments.
  Boundaries are inclusive, with respect to `from-date` and `until-date`."
  [freq from-date until-date]
  (let [datetimes (get-consecutive-datetimes from-date until-date),
        fragment-length (get-habit-goal-fragment-length freq)]
    (partition-all fragment-length datetimes)))

(defn create-habit-goal-fragment
  "Constructs a habit goal fragment from a list of consecutive DateTimes.
  `datetimes` should be nonempty, be sorted increasingly by date, and correspond to a habit goal fragment.
  Initializes `:total-done` to 0 and `:successful` to false."
  [datetimes]
  {:start-date (first datetimes),
   :end-date (last datetimes),
   :total-done 0,
   :successful false})

(defn span-of-habit-goal-fragment
  "Gets the number of days spanned by `habit-goal-fragment`."
  [habit-goal-fragment]
  (days-spanned-between-datetimes (:start-date habit-goal-fragment) (:end-date habit-goal-fragment)))

(defn during-habit-goal-fragment?
  "Returns true iff `datetime` occurs during `habit-goal-fragment`."
  [datetime habit-goal-fragment]
  (and (date-geq? datetime (:start-date habit-goal-fragment))
       (date-leq? datetime (:end-date habit-goal-fragment))))

(defn get-habit-data-during-fragment
  "Finds all `habit_day_record`s in `habit-data` that occur during `habit-goal-fragment`."
  [habit-data habit-goal-fragment]
  (filter #(during-habit-goal-fragment? (:date %) habit-goal-fragment) habit-data))

(defn evaluate-habit-goal-fragment-total-done
  "Updates the `:total-done` field of `habit-goal-fragment` based on a list of `habit_day_record`s."
  [habit-goal-fragment habit-data-during-fragment]
  (reduce #(update %1 :total-done + (:amount %2))
          habit-goal-fragment
          habit-data-during-fragment))

(defn evaluate-habit-goal-fragment-successful
  "Evaluates the `:successful` field of `habit-goal-fragment` based on its `:total-done` field, the type of habit, and the habit's goal."
  [habit-goal-fragment habit-type freq]
  (let [goal-amount (get-habit-goal-amount-for-datetime (:start-date habit-goal-fragment) freq)]
    (assoc habit-goal-fragment
           :successful ((if (= habit-type "good_habit") >= <=)
                        (:total-done habit-goal-fragment)
                        goal-amount))))

(defn evaluate-habit-goal-fragment
  "Evaluates `:total-done` and `:successful` fields of a habit goal fragment."
  [habit-goal-fragment habit-data habit-type freq]
  (let [habit-data-during-fragment (get-habit-data-during-fragment habit-data habit-goal-fragment)]
    (-> habit-goal-fragment
        (evaluate-habit-goal-fragment-total-done habit-data-during-fragment)
        (evaluate-habit-goal-fragment-successful habit-type freq))))

(defn get-habit-goal-fragments
  "Creates and evaluates habit goal fragments for a habit based on data from `current-date` or earlier.
  Returns `nil` if `sorted-habit-data` is empty, i.e. the habit has no relevant data."
  [sorted-habit-data current-date habit-type freq]
  (if-not (empty? sorted-habit-data)
    (let [habit-start-date (get-habit-start-date sorted-habit-data freq)
          partitioned-datetimes (partition-datetimes-based-on-habit-goal freq habit-start-date current-date)
          habit-goal-fragments (map #(create-habit-goal-fragment %) partitioned-datetimes)]
      (map #(evaluate-habit-goal-fragment % sorted-habit-data habit-type freq) habit-goal-fragments))))

(defn update-freq-stats-with-past-fragment
  "Updates fields of a `habit_frequency_stats` based on a past habit goal fragment."
  [habit-frequency-stats habit-goal-fragment]
  (let [successful (:successful habit-goal-fragment)]
    (as-> habit-frequency-stats $
          (update $ :total_fragments inc)
          (update $ :successful_fragments (if successful inc identity))
          (update $ :total_done + (:total-done habit-goal-fragment))
          (update $ :current_fragment_streak (if successful inc (constantly 0)))
          (assoc $ :best_fragment_streak (max (:current_fragment_streak $) (:best_fragment_streak $))))))

(defn update-freq-stats-with-current-fragment
  "Updates fields of a `habit_frequency_stats` based on the current habit goal fragment and its goal `freq`.
  Computes `:current_fragment_days_left` based on the fragment length defined by `freq` minus the span
  of `current-fragment`, whose `:end-date` field was cut short at the current date during construction.
  Only ever treats the current fragment as successful for good habits, and only ever treats it as failed for bad
  habits; we don't punish unfinished good habits or reward unfinished bad habits."
  [freq-stats current-fragment freq habit-type]
  (let [treat-as-successful (and (= habit-type "good_habit") (:successful current-fragment))
        treat-as-failed (and (= habit-type "bad_habit") (not (:successful current-fragment)))]
    (as-> freq-stats $
          (update $ :total_fragments (if (or treat-as-successful treat-as-failed) inc identity))
          (update $ :successful_fragments (if treat-as-successful inc identity))
          (update $ :total_done + (:total-done current-fragment))
          (update $ :current_fragment_streak (if treat-as-successful
                                               inc
                                               (if treat-as-failed (constantly 0) identity)))
          (assoc $ :best_fragment_streak (max (:current_fragment_streak $) (:best_fragment_streak $)))
          (assoc $ :current_fragment_total (:total-done current-fragment))
          (assoc $ :current_fragment_goal (get-habit-goal-amount-for-datetime (:start-date current-fragment) freq))
          (assoc $ :current_fragment_days_left (- (get-habit-goal-fragment-length freq)
                                                  (span-of-habit-goal-fragment current-fragment))))))

(defn compute-freq-stats-from-habit-goal-fragments
  "Computes a `habit_frequency_stats` based on a list of habit goal fragments with goal `freq`."
  [habit-goal-fragments habit freq]
  (let [past-fragments (butlast habit-goal-fragments)
        current-fragment (last habit-goal-fragments)]
    (as-> (assoc default-frequency-stats :habit_id (:_id habit)) freq-stats
          (reduce update-freq-stats-with-past-fragment
                  freq-stats
                  past-fragments)
          (update-freq-stats-with-current-fragment freq-stats current-fragment freq (:type_name habit)))))

(defn get-freq-stats-for-habit
  "Computes a `habit_frequency_stats` for a habit based on habit data from `current-date` or earlier."
  [db habit all-habit-data-until-current-date current-date]
  (let [sorted-habit-data (->> all-habit-data-until-current-date
                               (filter #(= (:habit_id %) (:_id habit)))
                               (sort-by :date)),
        freq (get-frequency habit),
        habit-goal-fragments (get-habit-goal-fragments sorted-habit-data current-date (:type_name habit) freq)]
    (if (nil? habit-goal-fragments)
      (assoc default-frequency-stats :habit_id (:_id habit))
      (compute-freq-stats-from-habit-goal-fragments habit-goal-fragments habit freq))))
