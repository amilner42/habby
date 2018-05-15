(ns api.freq-stats-util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clj-time.core :as t]
            [api.freq-stats-util :as freq-stats-util]
            [api.dt-util-test :as dt-util-test])
  (:import org.bson.types.ObjectId))

(def number-of-test-check-iterations 30)

(def generate-random-specific-day-of-week-frequency
  (gen/hash-map :type_name (gen/return "specific_day_of_week_frequency"),
                :monday gen/nat, :tuesday gen/nat, :wednesday gen/nat, :thursday gen/nat,
                :friday gen/nat, :saturday gen/nat, :sunday gen/nat))

(def generate-random-total-week-frequency
  (gen/hash-map :type_name (gen/return "total_week_frequency"),
                :week gen/nat))

(def generate-random-every-x-days-frequency
  (gen/hash-map :type_name (gen/return "every_x_days_frequency"),
                :times gen/nat, :days gen/s-pos-int))

; Generates a random ObjectId. `(gen/return (ObjectId.))` always returns the same ID, this avoids that.
(def generate-random-ID
  (gen/fmap (fn [_] (ObjectId.)) gen/int))

(defn generate-random-habit-day-record
  "Generates a `habit_day_record`, with each field created by its supplied generator, or randomly generated if not supplied."
  [{:keys [gen-id gen-habit-id gen-date gen-amount],
    :or {gen-id generate-random-ID,
         gen-habit-id generate-random-ID,
         gen-date dt-util-test/generate-random-datetime,
         gen-amount gen/nat}}]
  (gen/hash-map :_id gen-id,
                :habit_id gen-habit-id,
                :date gen-date,
                :amount gen-amount))

(defn random-habit-day-record
  "Like `generate-random-habit-day-record`, but returns a `habit_day_record` instead of a generator for one.
  Args should still be generators, as in `generate-random-habit-day-record`."
  [args]
  (gen/generate (generate-random-habit-day-record args)))

(defn generate-random-habit-goal-fragment
  "Generates a habit goal fragment, with each field created by its supplied generator, or randomly generated if not supplied.
  If either `gen-start-date` or `gen-end-date` is not supplied, generates both dates and makes sure `start-date` comes before `end-date`."
  [{:keys [gen-start-date gen-end-date gen-total-done gen-successful],
    :or {gen-total-done gen/nat,
         gen-successful gen/boolean}}]
  (if (or (nil? gen-start-date) (nil? gen-end-date))
    (gen/let [[start-date end-date] dt-util-test/generate-two-random-sorted-datetimes]
      (gen/hash-map :start-date (gen/return start-date),
                    :end-date (gen/return end-date),
                    :total-done gen-total-done,
                    :successful gen-successful))
    (gen/hash-map :start-date gen-start-date,
                  :end-date gen-end-date,
                  :total-done gen-total-done,
                  :successful gen-successful)))

(defn random-habit-goal-fragment
  "Like `generate-random-habit-goal-fragment`, but returns a habit goal fragment instead of a generator for one.
  Args should still be generators, as in `generate-random-habit-goal-fragment`."
  [args]
  (gen/generate (generate-random-habit-goal-fragment args)))

(defspec get-habit-goal-fragment-length-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-specific-day-of-week-frequency]
           (= 1 (freq-stats-util/get-habit-goal-fragment-length freq))))

(defspec get-habit-goal-fragment-length-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-total-week-frequency]
           (= 7 (freq-stats-util/get-habit-goal-fragment-length freq))))

(defspec get-habit-goal-fragment-length-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-every-x-days-frequency]
           (= (:days freq) (freq-stats-util/get-habit-goal-fragment-length freq))))

(defspec get-habit-goal-amount-for-datetime-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-specific-day-of-week-frequency,
                        dt dt-util-test/generate-random-monday-datetime]
           (and (= (:monday freq) (freq-stats-util/get-habit-goal-amount-for-datetime dt freq))
                (= (:tuesday freq) (freq-stats-util/get-habit-goal-amount-for-datetime (t/plus dt (t/days 1)) freq))
                (= (:wednesday freq) (freq-stats-util/get-habit-goal-amount-for-datetime (t/plus dt (t/days 2)) freq))
                (= (:thursday freq) (freq-stats-util/get-habit-goal-amount-for-datetime (t/plus dt (t/days 3)) freq))
                (= (:friday freq) (freq-stats-util/get-habit-goal-amount-for-datetime (t/plus dt (t/days 4)) freq))
                (= (:saturday freq) (freq-stats-util/get-habit-goal-amount-for-datetime (t/plus dt (t/days 5)) freq))
                (= (:sunday freq) (freq-stats-util/get-habit-goal-amount-for-datetime (t/plus dt (t/days 6)) freq)))))

(defspec get-habit-goal-amount-for-datetime-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-total-week-frequency,
                        dt dt-util-test/generate-random-datetime]
           (= (:week freq) (freq-stats-util/get-habit-goal-amount-for-datetime dt freq))))

(defspec get-habit-goal-amount-for-datetime-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-every-x-days-frequency,
                        dt dt-util-test/generate-random-datetime]
           (= (:times freq) (freq-stats-util/get-habit-goal-amount-for-datetime dt freq))))

(defspec get-habit-start-date-total-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [total-week-frequency generate-random-total-week-frequency,
                        monday-dt dt-util-test/generate-random-monday-datetime]
           (let [same-week-sorted-datetimes (sort (gen/generate (gen/not-empty (gen/vector (gen/fmap #(t/plus monday-dt (t/days %))
                                                                                                     (gen/choose 0 6))))))
                 same-week-sorted-habit-data (map #(random-habit-day-record {:gen-date (gen/return %)}) same-week-sorted-datetimes)]
             (= monday-dt
                (freq-stats-util/get-habit-start-date same-week-sorted-habit-data total-week-frequency)))))

(defspec get-habit-start-date-other-frequencies-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        every-x-days-frequency generate-random-every-x-days-frequency]
           (let [sorted-datetimes (sort (gen/generate (gen/not-empty (gen/vector dt-util-test/generate-random-datetime))))
                 sorted-habit-data (map #(random-habit-day-record {:gen-date (gen/return %)}) sorted-datetimes)]
             (= (first sorted-datetimes)
                (freq-stats-util/get-habit-start-date sorted-habit-data specific-day-of-week-frequency)
                (freq-stats-util/get-habit-start-date sorted-habit-data every-x-days-frequency)))))

(defspec partition-datetimes-based-on-habit-goal-count-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        total-week-frequency generate-random-total-week-frequency,
                        every-x-days-frequency generate-random-every-x-days-frequency,
                        {:keys [from-date until-date days-apart]} dt-util-test/generate-two-random-datetimes-with-days-apart]
           (let [total-span (inc days-apart)]
             (and (= total-span
                     (count (freq-stats-util/partition-datetimes-based-on-habit-goal specific-day-of-week-frequency from-date until-date)))
                  (== (Math/ceil (/ total-span 7))
                      (count (freq-stats-util/partition-datetimes-based-on-habit-goal total-week-frequency from-date until-date)))
                  (== (Math/ceil (/ total-span (:days every-x-days-frequency)))
                      (count (freq-stats-util/partition-datetimes-based-on-habit-goal every-x-days-frequency from-date until-date)))))))

(defspec partition-datetimes-based-on-habit-goal-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        {:keys [from-date until-date days-apart]} dt-util-test/generate-two-random-datetimes-with-days-apart]
           (let [from-date-at-start-of-day (t/with-time-at-start-of-day from-date)]
             (= (map #(-> from-date-at-start-of-day
                          (t/plus (t/days %))
                          vector)
                     (range (inc days-apart)))
                (freq-stats-util/partition-datetimes-based-on-habit-goal specific-day-of-week-frequency from-date until-date)))))

(defspec create-habit-goal-fragment-test
         number-of-test-check-iterations
         (prop/for-all [[start-date end-date :as datetimes] dt-util-test/generate-two-random-sorted-datetimes
                        single-date dt-util-test/generate-random-datetime]
           (and (= {:start-date start-date, :end-date end-date, :total-done 0, :successful false}
                   (freq-stats-util/create-habit-goal-fragment datetimes))
                (= {:start-date single-date, :end-date single-date, :total-done 0, :successful false}
                   (freq-stats-util/create-habit-goal-fragment [single-date])))))

(defspec span-of-habit-goal-fragment-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date days-apart]} dt-util-test/generate-two-random-datetimes-with-days-apart]
           (let [habit-goal-fragment (random-habit-goal-fragment {:gen-start-date (gen/return from-date)
                                                                  :gen-end-date (gen/return until-date)})]
             (= (inc days-apart)
                (freq-stats-util/span-of-habit-goal-fragment habit-goal-fragment)))))

(defspec during-habit-goal-fragment?-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date days-apart]} dt-util-test/generate-two-random-datetimes-with-days-apart,
                        days-to-add gen/int]
           (let [habit-goal-fragment (random-habit-goal-fragment {:gen-start-date (gen/return from-date)
                                                                  :gen-end-date (gen/return until-date)}),
                 datetime (t/plus (gen/generate (dt-util-test/generate-random-datetime-on-given-date from-date))
                                  (t/days days-to-add))]
             (= (<= 0 days-to-add days-apart)
                (freq-stats-util/during-habit-goal-fragment? datetime habit-goal-fragment)))))

(defspec get-habit-data-during-fragment-test
         number-of-test-check-iterations
         (prop/for-all [days-to-subtract gen/s-pos-int,
                        days-to-add gen/s-pos-int,
                        habit-goal-fragment (generate-random-habit-goal-fragment {})]
           (let [start-date (:start-date habit-goal-fragment),
                 end-date (:end-date habit-goal-fragment),
                 too-early-dt (t/minus start-date (t/days days-to-subtract)),
                 outside-range-record-a (random-habit-day-record {:gen-date (gen/return too-early-dt)}),
                 within-range-record-a (random-habit-day-record {:gen-date (gen/return start-date)}),
                 within-range-record-b (random-habit-day-record {:gen-date (gen/return end-date)}),
                 too-late-dt (t/plus end-date (t/days days-to-add)),
                 outside-range-record-b (random-habit-day-record {:gen-date (gen/return too-late-dt)}),
                 habit-data [outside-range-record-a,
                             within-range-record-a,
                             within-range-record-b,
                             outside-range-record-b]]
             (= [within-range-record-a, within-range-record-b]
                (freq-stats-util/get-habit-data-during-fragment habit-data habit-goal-fragment)))))

(defspec evaluate-habit-goal-fragment-total-done-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (generate-random-habit-goal-fragment {}),
                        amounts (gen/vector gen/nat)]
           (let [habit-data-during-fragment (map #(random-habit-day-record {:gen-amount (gen/return %)})
                                                 amounts)]
             (= (update habit-goal-fragment :total-done + (apply + amounts))
                (freq-stats-util/evaluate-habit-goal-fragment-total-done habit-goal-fragment habit-data-during-fragment)))))

(defspec evaluate-habit-goal-fragment-successful-test
         number-of-test-check-iterations
         (prop/for-all [habit-goal-fragment (generate-random-habit-goal-fragment {}),
                        total-week-frequency generate-random-total-week-frequency]
           (let [habit-type "bad_habit"]
             (= (assoc habit-goal-fragment :successful (<= (:total-done habit-goal-fragment) (:week total-week-frequency)))
                (freq-stats-util/evaluate-habit-goal-fragment-successful habit-goal-fragment habit-type total-week-frequency)))))
