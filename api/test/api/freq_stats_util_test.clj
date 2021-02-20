(ns api.freq-stats-util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clj-time.core :as t]
            [api.freq-stats-util :as freq-stats-util]
            [api.dt-util :as dt-util]
            [api.dt-util-test :as dt-util-test]
            [api.habit-util :as habit-util])
  (:import org.bson.types.ObjectId))

(def number-of-test-check-iterations 30)

;; Generators
;; ---------------------------------------------------------------------------

(defn create-specific-day-of-week-frequency
  "Creates a `specific_day_of_week_frequency` from a vector of numbers corresponding to Monday to Sunday amounts."
  [week-amount-vector]
  (assoc (zipmap [:monday :tuesday :wednesday :thursday :friday :saturday :sunday] week-amount-vector)
         :type_name "specific_day_of_week_frequency"))

(def gen-specific-day-of-week-frequency
  (gen/fmap create-specific-day-of-week-frequency (gen/vector gen/nat 7)))

(def gen-total-week-frequency
  (gen/hash-map :type_name (gen/return "total_week_frequency"),
                :week gen/nat))

(def gen-every-x-days-frequency
  (gen/hash-map :type_name (gen/return "every_x_days_frequency"),
                :times gen/nat, :days gen/s-pos-int))

(def gen-frequency
  (gen/one-of [gen-specific-day-of-week-frequency,
               gen-total-week-frequency,
               gen-every-x-days-frequency]))

; Generates a random ObjectId. `(gen/return (ObjectId.))` always returns the same ID, this avoids that.
(def gen-objectId
  (gen/fmap (fn [_] (ObjectId.)) gen/int))

(defn gen-habit-day-record
  "Generates a `habit_day_record`, with each field created by its supplied generator, or randomly generated if not supplied."
  [{:keys [gen-id gen-habit-id gen-date gen-amount],
    :or {gen-id gen-objectId,
         gen-habit-id gen-objectId,
         gen-date dt-util-test/gen-datetime,
         gen-amount gen/nat}}]
  (gen/hash-map :_id gen-id,
                :habit_id gen-habit-id,
                :date gen-date,
                :amount gen-amount))

(defn random-habit-day-record
  "Like `gen-habit-day-record`, but returns a `habit_day_record` instead of a generator for one.
  Args should still be generators, as in `gen-habit-day-record`."
  [args]
  (gen/generate (gen-habit-day-record args)))

(defn gen-habit-goal-fragment
  "Generates a habit goal fragment, with each field created by its supplied generator, or randomly generated if not supplied.
  If either `gen-start-date` or `gen-end-date` is not supplied, generates both dates and makes sure `start-date` comes before `end-date`."
  [{:keys [gen-start-date gen-end-date gen-total-done gen-successful],
    :or {gen-total-done gen/nat,
         gen-successful gen/boolean}}]
  (if (or (nil? gen-start-date) (nil? gen-end-date))
    (gen/let [[start-date end-date] dt-util-test/gen-two-sorted-datetimes]
      (gen/hash-map :start-date (gen/return start-date),
                    :end-date (gen/return end-date),
                    :total-done gen-total-done,
                    :successful gen-successful))
    (gen/hash-map :start-date gen-start-date,
                  :end-date gen-end-date,
                  :total-done gen-total-done,
                  :successful gen-successful)))

(defn random-habit-goal-fragment
  "Like `gen-habit-goal-fragment`, but returns a habit goal fragment instead of a generator for one.
  Args should still be generators, as in `gen-habit-goal-fragment`."
  [args]
  (gen/generate (gen-habit-goal-fragment args)))

(defn gen-habit-frequency-stats
  "Generates a `habit_frequency_stats`, with each field created by its supplied generator, or randomly generated if not supplied."
  [{:keys [gen-habit-id gen-total-fragments gen-successful-fragments gen-total-done gen-current-fragment-streak gen-best-fragment-streak
           gen-current-fragment-total gen-current-fragment-goal gen-current-fragment-days-left],
    :or {gen-habit-id gen-objectId,
         gen-total-fragments gen/nat,
         gen-successful-fragments gen/nat,
         gen-total-done gen/nat,
         gen-current-fragment-streak gen/nat,
         gen-best-fragment-streak gen/nat,
         gen-current-fragment-total gen/nat,
         gen-current-fragment-goal gen/nat,
         gen-current-fragment-days-left gen/nat}}]
  (gen/such-that #(<= (:current_fragment_streak %) (:best_fragment_streak %))
                 (gen/hash-map :habit_id gen-habit-id,
                               :total_fragments gen-total-fragments,
                               :successful_fragments gen-successful-fragments,
                               :total_done gen-total-done,
                               :current_fragment_streak gen-current-fragment-streak,
                               :best_fragment_streak gen-best-fragment-streak,
                               :current_fragment_total gen-current-fragment-total,
                               :current_fragment_goal gen-current-fragment-goal,
                               :current_fragment_days_left gen-current-fragment-days-left)))

(def gen-good-habit
  (gen/hash-map :type_name (gen/return "good_habit"),
                :_id gen-objectId,
                :suspended gen/boolean,
                :name gen/string,
                :description gen/string,
                :unit_name_singular gen/string,
                :unit_name_plural gen/string,
                :time_of_day (gen/elements [:MORNING :EVENING :ANYTIME]),
                :target_frequency gen-frequency))

(def gen-bad-habit
  (gen/hash-map :type_name (gen/return "bad_habit"),
                :_id gen-objectId,
                :suspended gen/boolean,
                :name gen/string,
                :description gen/string,
                :unit_name_singular gen/string,
                :unit_name_plural gen/string,
                :threshold_frequency gen-frequency))

(def gen-habit
  (gen/one-of [gen-good-habit, gen-bad-habit]))

;; Tests
;; ---------------------------------------------------------------------------

(defspec get-habit-goal-fragment-length-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency gen-specific-day-of-week-frequency]
           (= 1 (freq-stats-util/get-habit-goal-fragment-length specific-day-of-week-frequency))))

(defspec get-habit-goal-fragment-length-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [total-week-frequency gen-total-week-frequency]
           (= 7 (freq-stats-util/get-habit-goal-fragment-length total-week-frequency))))

(defspec get-habit-goal-fragment-length-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [every-x-days-frequency gen-every-x-days-frequency]
           (= (:days every-x-days-frequency)
              (freq-stats-util/get-habit-goal-fragment-length every-x-days-frequency))))

(defspec get-habit-goal-amount-for-datetime-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [week-amount-vector (gen/vector gen/nat 7),
                        monday-dt dt-util-test/gen-monday-datetime,
                        num-days-later (gen/choose 0 6)]
           (let [later-in-week-dt (t/plus monday-dt (t/days num-days-later)),
                 specific-day-of-week-frequency (create-specific-day-of-week-frequency week-amount-vector)]
             (= (nth week-amount-vector num-days-later)
                (freq-stats-util/get-habit-goal-amount-for-datetime later-in-week-dt specific-day-of-week-frequency)))))

(defspec get-habit-goal-amount-for-datetime-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [total-week-frequency gen-total-week-frequency,
                        dt dt-util-test/gen-datetime]
           (= (:week total-week-frequency) (freq-stats-util/get-habit-goal-amount-for-datetime dt total-week-frequency))))

(defspec get-habit-goal-amount-for-datetime-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [every-x-days-frequency gen-every-x-days-frequency,
                        dt dt-util-test/gen-datetime]
           (= (:times every-x-days-frequency) (freq-stats-util/get-habit-goal-amount-for-datetime dt every-x-days-frequency))))

(defspec get-habit-start-date-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [total-week-frequency gen-total-week-frequency,
                        monday-dt dt-util-test/gen-monday-datetime]
           (let [generate-same-week-datetime (gen/fmap #(t/plus monday-dt (t/days %)) (gen/choose 0 6))
                 same-week-sorted-datetimes (sort (gen/generate (gen/not-empty (gen/vector generate-same-week-datetime))))
                 same-week-sorted-habit-data (map #(random-habit-day-record {:gen-date (gen/return %)}) same-week-sorted-datetimes)]
             (= monday-dt
                (freq-stats-util/get-habit-start-date same-week-sorted-habit-data total-week-frequency)))))

(defspec get-habit-start-date-other-frequencies-test
         number-of-test-check-iterations
         (prop/for-all [freq (gen/one-of [gen-specific-day-of-week-frequency, gen-every-x-days-frequency])]
           (let [sorted-datetimes (sort (gen/generate (gen/not-empty (gen/vector dt-util-test/gen-datetime))))
                 sorted-habit-data (map #(random-habit-day-record {:gen-date (gen/return %)}) sorted-datetimes)]
             (= (first sorted-datetimes)
                (freq-stats-util/get-habit-start-date sorted-habit-data freq)))))

(defspec partition-datetimes-based-on-habit-goal-count-test
         number-of-test-check-iterations
         (prop/for-all [freq gen-frequency,
                        {:keys [from-date until-date num-days-apart]} dt-util-test/gen-two-datetimes-with-num-days-apart]
           (let [total-span (inc num-days-apart),
                 fragment-length (freq-stats-util/get-habit-goal-fragment-length freq)]
             (== (Math/ceil (/ total-span fragment-length))
                 (count (freq-stats-util/partition-datetimes-based-on-habit-goal freq from-date until-date))))))

(defspec partition-datetimes-based-on-habit-goal-two-fragments-test
         number-of-test-check-iterations
         (prop/for-all [freq gen-frequency,
                        from-date dt-util-test/gen-datetime]
           (let [fragment-length (freq-stats-util/get-habit-goal-fragment-length freq),
                 num-days-in-remaining-fragment (gen/generate (gen/choose 1 fragment-length)),
                 num-days-later (dec (+ fragment-length num-days-in-remaining-fragment)),
                 until-date (dt-util-test/random-datetime-on-given-date (t/plus from-date (t/days num-days-later))),
                 from-date-at-start-of-day (t/with-time-at-start-of-day from-date),
                 first-fragment-dates (map #(t/plus from-date-at-start-of-day (t/days %)) (range fragment-length)),
                 remaining-fragment-dates (map #(t/plus from-date-at-start-of-day (t/days %)) (range fragment-length (inc num-days-later)))]
             (= [first-fragment-dates, remaining-fragment-dates]
                (freq-stats-util/partition-datetimes-based-on-habit-goal freq from-date until-date)))))

(defspec create-habit-goal-fragment-single-date-fragment-test
         number-of-test-check-iterations
         (prop/for-all [single-date dt-util-test/gen-datetime]
           (= {:start-date single-date, :end-date single-date, :total-done 0, :successful false}
              (freq-stats-util/create-habit-goal-fragment [single-date]))))

(defspec create-habit-goal-fragment-multiple-date-fragment-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} dt-util-test/gen-two-datetimes-with-num-days-apart]
           (let [middle-dates (gen/generate (gen/vector (gen/fmap #(t/plus from-date (t/days %)) (gen/choose 0 num-days-apart)))),
                 datetimes (cons from-date (conj middle-dates until-date))]
             (= {:start-date from-date, :end-date until-date, :total-done 0, :successful false}
                (freq-stats-util/create-habit-goal-fragment datetimes)))))

(defspec span-of-habit-goal-fragment-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} dt-util-test/gen-two-datetimes-with-num-days-apart]
           (let [habit-goal-fragment (random-habit-goal-fragment {:gen-start-date (gen/return from-date)
                                                                  :gen-end-date (gen/return until-date)})]
             (= (inc num-days-apart)
                (freq-stats-util/span-of-habit-goal-fragment habit-goal-fragment)))))

(defspec during-habit-goal-fragment?-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} dt-util-test/gen-two-datetimes-with-num-days-apart,
                        num-days-to-add gen/int]
           (let [habit-goal-fragment (random-habit-goal-fragment {:gen-start-date (gen/return from-date)
                                                                  :gen-end-date (gen/return until-date)}),
                 datetime (t/plus (dt-util-test/random-datetime-on-given-date from-date)
                                  (t/days num-days-to-add))]
             (= (<= 0 num-days-to-add num-days-apart)
                (freq-stats-util/during-habit-goal-fragment? datetime habit-goal-fragment)))))

(defspec get-habit-data-during-fragment-test
         number-of-test-check-iterations
         (prop/for-all [num-days-earlier gen/s-pos-int,
                        num-days-later gen/s-pos-int,
                        habit-goal-fragment (gen-habit-goal-fragment {})]
           (let [start-date (:start-date habit-goal-fragment),
                 end-date (:end-date habit-goal-fragment),
                 too-early-dt (t/minus start-date (t/days num-days-earlier)),
                 outside-range-record-a (random-habit-day-record {:gen-date (gen/return too-early-dt)}),
                 within-range-record-a (random-habit-day-record {:gen-date (gen/return start-date)}),
                 within-range-record-b (random-habit-day-record {:gen-date (gen/return end-date)}),
                 too-late-dt (t/plus end-date (t/days num-days-later)),
                 outside-range-record-b (random-habit-day-record {:gen-date (gen/return too-late-dt)}),
                 habit-data [outside-range-record-a,
                             within-range-record-a,
                             within-range-record-b,
                             outside-range-record-b]]
             (= [within-range-record-a, within-range-record-b]
                (freq-stats-util/get-habit-data-during-fragment habit-data habit-goal-fragment)))))

(defspec get-habit-data-during-fragment-within-range-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (gen-habit-goal-fragment {})]
           (let [start-date (:start-date habit-goal-fragment),
                 end-date (:end-date habit-goal-fragment),
                 gen-within-range-dt (gen/fmap #(t/plus start-date (t/days %))
                                               (gen/choose 0 (dec (dt-util/days-spanned-between-datetimes start-date end-date)))),
                 within-range-data (gen/generate (gen/vector (gen-habit-day-record {:gen-date gen-within-range-dt})))]
             (= within-range-data
                (freq-stats-util/get-habit-data-during-fragment within-range-data habit-goal-fragment)))))

(defspec get-habit-data-during-fragment-outside-range-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (gen-habit-goal-fragment {})]
           (let [start-date (:start-date habit-goal-fragment),
                 end-date (:end-date habit-goal-fragment),
                 gen-outside-range-dt (gen/one-of [(gen/fmap #(t/minus start-date (t/days %)) gen/s-pos-int),
                                                   (gen/fmap #(t/plus end-date (t/days %)) gen/s-pos-int)]),
                 outside-range-data (gen/generate (gen/vector (gen-habit-day-record {:gen-date gen-outside-range-dt})))]
             (= []
                (freq-stats-util/get-habit-data-during-fragment outside-range-data habit-goal-fragment)))))

(defspec evaluate-habit-goal-fragment-total-done-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (gen-habit-goal-fragment {}),
                        amounts (gen/vector gen/nat)]
           (let [habit-data-during-fragment (map #(random-habit-day-record {:gen-amount (gen/return %)})
                                                 amounts)]
             (= (update habit-goal-fragment :total-done + (apply + amounts))
                (freq-stats-util/evaluate-habit-goal-fragment-total-done habit-goal-fragment habit-data-during-fragment)))))

(defspec evaluate-habit-goal-fragment-successful-bad-habit-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (gen-habit-goal-fragment {}),
                        total-week-frequency gen-total-week-frequency]
           (let [habit-type "bad_habit"]
             (= (assoc habit-goal-fragment :successful (<= (:total-done habit-goal-fragment) (:week total-week-frequency)))
                (freq-stats-util/evaluate-habit-goal-fragment-successful habit-goal-fragment habit-type total-week-frequency)))))

(defspec evaluate-habit-goal-fragment-successful-good-habit-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (gen-habit-goal-fragment {}),
                        every-x-days-frequency gen-every-x-days-frequency]
           (let [habit-type "good_habit"]
             (= (assoc habit-goal-fragment :successful (>= (:total-done habit-goal-fragment) (:times every-x-days-frequency)))
                (freq-stats-util/evaluate-habit-goal-fragment-successful habit-goal-fragment habit-type every-x-days-frequency)))))

(defspec get-habit-goal-fragments-bad-habit-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-start-date dt-util-test/gen-datetime,
                        num-days-later gen/nat,
                        week-amount-vector (gen/vector gen/nat 7)]
           (let [specific-day-of-week-frequency (create-specific-day-of-week-frequency week-amount-vector),
                 habit-type "bad_habit",
                 current-date (t/plus habit-start-date (t/days num-days-later)),
                 habit-record-dates (dt-util/get-consecutive-datetimes habit-start-date current-date),
                 habit-record-amounts (gen/generate (gen/vector gen/nat (inc num-days-later))),
                 habit-record-days-of-week (map t/day-of-week habit-record-dates),
                 sorted-habit-data (map #(random-habit-day-record {:gen-date (gen/return %1)
                                                                   :gen-amount (gen/return %2)})
                                        habit-record-dates
                                        habit-record-amounts)]
             (= (map (fn [date amount day-of-week]
                       {:start-date date,
                        :end-date date,
                        :total-done amount,
                        :successful (<= amount (nth week-amount-vector (dec day-of-week)))})
                     habit-record-dates
                     habit-record-amounts
                     habit-record-days-of-week)
                (freq-stats-util/get-habit-goal-fragments sorted-habit-data current-date habit-type specific-day-of-week-frequency)))))

(defspec get-habit-goal-fragments-good-habit-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-start-date dt-util-test/gen-datetime,
                        num-days-later gen/nat,
                        every-x-days-frequency gen-every-x-days-frequency]
           (let [habit-type "good_habit",
                 current-date (t/plus habit-start-date (t/days num-days-later)),
                 habit-record-dates (dt-util/get-consecutive-datetimes habit-start-date current-date),
                 habit-record-amounts (gen/generate (gen/vector gen/nat (inc num-days-later))),
                 sorted-habit-data (map #(random-habit-day-record {:gen-date (gen/return %1)
                                                                   :gen-amount (gen/return %2)})
                                        habit-record-dates
                                        habit-record-amounts)
                 partitioned-habit-record-dates (partition-all (:days every-x-days-frequency) habit-record-dates)
                 partitioned-habit-record-amounts (partition-all (:days every-x-days-frequency) habit-record-amounts)]
             (= (map (fn [dates amounts]
                       (let [amounts-sum (reduce + amounts)]
                         {:start-date (first dates),
                          :end-date (last dates),
                          :total-done amounts-sum,
                          :successful (>= amounts-sum (:times every-x-days-frequency))}))
                     partitioned-habit-record-dates
                     partitioned-habit-record-amounts)
                (freq-stats-util/get-habit-goal-fragments sorted-habit-data current-date habit-type every-x-days-frequency)))))

(defspec update-freq-stats-with-past-fragment-failed-fragment-test
         number-of-test-check-iterations
         (prop/for-all [habit-frequency-stats (gen-habit-frequency-stats {}),
                        failed-habit-goal-fragment (gen-habit-goal-fragment {:gen-successful (gen/return false)})]
           (let [total-done (:total-done failed-habit-goal-fragment)]
             (= (-> habit-frequency-stats
                    (update :total_fragments inc)
                    (update :total_done + total-done)
                    (assoc :current_fragment_streak 0))
                (freq-stats-util/update-freq-stats-with-past-fragment habit-frequency-stats failed-habit-goal-fragment)))))

(defspec update-freq-stats-with-past-fragment-successful-fragment-test
         number-of-test-check-iterations
         (prop/for-all [habit-frequency-stats (gen-habit-frequency-stats {}),
                        successful-habit-goal-fragment (gen-habit-goal-fragment {:gen-successful (gen/return true)})]
           (let [total-done (:total-done successful-habit-goal-fragment),
                 current-streak-is-best-streak (= (:current_fragment_streak habit-frequency-stats)
                                                  (:best_fragment_streak habit-frequency-stats))]
             (= (-> habit-frequency-stats
                    (update :total_fragments inc)
                    (update :successful_fragments inc)
                    (update :total_done + total-done)
                    (update :current_fragment_streak inc)
                    (update :best_fragment_streak (if current-streak-is-best-streak inc identity)))
                (freq-stats-util/update-freq-stats-with-past-fragment habit-frequency-stats successful-habit-goal-fragment)))))

(defspec update-freq-stats-with-current-fragment-good-habit-successful-fragment-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-frequency-stats (gen-habit-frequency-stats {}),
                        total-week-frequency gen-total-week-frequency,
                        current-fragment-start-date dt-util-test/gen-datetime,
                        num-days-later (gen/choose 0 6)]
           (let [current-fragment-end-date (t/plus current-fragment-start-date (t/days num-days-later)),
                 successful-current-fragment (random-habit-goal-fragment {:gen-start-date (gen/return current-fragment-start-date),
                                                                          :gen-end-date (gen/return current-fragment-end-date),
                                                                          :gen-successful (gen/return true)}),
                 habit-type "good_habit",
                 total-done (:total-done successful-current-fragment),
                 current-streak-is-best-streak (= (:current_fragment_streak habit-frequency-stats)
                                                  (:best_fragment_streak habit-frequency-stats))]
             (= (-> habit-frequency-stats
                    (update :total_fragments inc)
                    (update :successful_fragments inc)
                    (update :total_done + total-done)
                    (update :current_fragment_streak inc)
                    (update :best_fragment_streak (if current-streak-is-best-streak inc identity))
                    (assoc :current_fragment_total total-done)
                    (assoc :current_fragment_goal (:week total-week-frequency))
                    (assoc :current_fragment_days_left (- 7 (inc num-days-later))))
                (freq-stats-util/update-freq-stats-with-current-fragment habit-frequency-stats
                                                                         successful-current-fragment
                                                                         total-week-frequency
                                                                         habit-type)))))

(defspec update-freq-stats-with-current-fragment-good-habit-failed-fragment-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-frequency-stats (gen-habit-frequency-stats {}),
                        every-x-days-frequency gen-every-x-days-frequency,
                        current-fragment-start-date dt-util-test/gen-datetime]
           (let [fragment-length (:days every-x-days-frequency),
                 num-days-later (gen/generate (gen/choose 0 (dec fragment-length)))
                 current-fragment-end-date (t/plus current-fragment-start-date (t/days num-days-later)),
                 failed-current-fragment (random-habit-goal-fragment {:gen-start-date (gen/return current-fragment-start-date),
                                                                      :gen-end-date (gen/return current-fragment-end-date),
                                                                      :gen-successful (gen/return false)}),
                 habit-type "good_habit",
                 total-done (:total-done failed-current-fragment)]
             (= (-> habit-frequency-stats
                    (update :total_done + total-done)
                    (assoc :current_fragment_total total-done)
                    (assoc :current_fragment_goal (:times every-x-days-frequency))
                    (assoc :current_fragment_days_left (- fragment-length (inc num-days-later))))
                (freq-stats-util/update-freq-stats-with-current-fragment habit-frequency-stats
                                                                         failed-current-fragment
                                                                         every-x-days-frequency
                                                                         habit-type)))))

(defspec update-freq-stats-with-current-fragment-bad-habit-failed-fragment-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-frequency-stats (gen-habit-frequency-stats {}),
                        specific-day-of-week-frequency gen-specific-day-of-week-frequency,
                        current-fragment-date dt-util-test/gen-datetime]
           (let [failed-current-fragment (random-habit-goal-fragment {:gen-start-date (gen/return current-fragment-date),
                                                                      :gen-end-date (gen/return current-fragment-date),
                                                                      :gen-successful (gen/return false)}),
                 habit-type "bad_habit",
                 total-done (:total-done failed-current-fragment)]
             (= (-> habit-frequency-stats
                    (update :total_fragments inc)
                    (update :total_done + total-done)
                    (assoc :current_fragment_streak 0)
                    (assoc :current_fragment_total total-done)
                    (assoc :current_fragment_goal (freq-stats-util/get-habit-goal-amount-for-datetime current-fragment-date
                                                                                                      specific-day-of-week-frequency))
                    (assoc :current_fragment_days_left 0))
                (freq-stats-util/update-freq-stats-with-current-fragment habit-frequency-stats
                                                                         failed-current-fragment
                                                                         specific-day-of-week-frequency
                                                                         habit-type)))))

(defspec update-freq-stats-with-current-fragment-bad-habit-successful-fragment-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [habit-frequency-stats (gen-habit-frequency-stats {}),
                        total-week-frequency gen-total-week-frequency,
                        current-fragment-start-date dt-util-test/gen-datetime,
                        num-days-later (gen/choose 0 6)]
           (let [current-fragment-end-date (t/plus current-fragment-start-date (t/days num-days-later)),
                 successful-current-fragment (random-habit-goal-fragment {:gen-start-date (gen/return current-fragment-start-date),
                                                                          :gen-end-date (gen/return current-fragment-end-date),
                                                                          :gen-successful (gen/return true),})
                 habit-type "bad_habit",
                 total-done (:total-done successful-current-fragment)]
             (= (-> habit-frequency-stats
                    (update :total_done + total-done)
                    (assoc :current_fragment_total total-done)
                    (assoc :current_fragment_goal (:week total-week-frequency))
                    (assoc :current_fragment_days_left (- 7 (inc num-days-later))))
                (freq-stats-util/update-freq-stats-with-current-fragment habit-frequency-stats
                                                                         successful-current-fragment
                                                                         total-week-frequency
                                                                         habit-type)))))

(defspec compute-freq-stats-from-habit-goal-fragments-total-done-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragments (gen/not-empty (gen/vector (gen-habit-goal-fragment {}))),
                        habit gen-habit]
           (let [total-done (reduce #(+ %1 (:total-done %2)) 0 habit-goal-fragments),
                 freq (habit-util/get-frequency habit)]
             (= total-done
                (:total_done (freq-stats-util/compute-freq-stats-from-habit-goal-fragments habit-goal-fragments
                                                                                           habit
                                                                                           freq))))))

(defspec get-freq-stats-for-habit-total-done-test
         number-of-test-check-iterations
         (prop/for-all [current-date dt-util-test/gen-datetime,
                        habit gen-habit]
           (let [gen-dt-before-current-date (gen/fmap #(t/minus current-date (t/days %)) gen/nat),
                 other-habits-data (gen/generate (gen/vector (gen-habit-day-record {:gen-habit-id (gen/such-that #(not= % (:_id habit))
                                                                                                                 gen-objectId),
                                                                                    :gen-date gen-dt-before-current-date}))),
                 this-habit-data (gen/generate (gen/vector (gen-habit-day-record {:gen-habit-id (gen/return (:_id habit)),
                                                                                  :gen-date gen-dt-before-current-date}))),
                 all-habit-data-until-current-date (concat other-habits-data this-habit-data),
                 this-habit-total-done (reduce #(+ %1 (:amount %2)) 0 this-habit-data)]
             (= this-habit-total-done
                (:total_done (freq-stats-util/get-freq-stats-for-habit habit all-habit-data-until-current-date current-date))))))
