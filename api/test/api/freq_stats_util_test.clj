(ns api.freq-stats-util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clj-time.core :as t]
            [api.freq-stats-util :as freq-stats-util]
            [api.dt-util-test :refer [generate-random-datetime, generate-random-monday-datetime, generate-two-random-sorted-datetimes,
                                      generate-two-random-datetimes-with-days-apart, generate-random-datetime-on-given-date]])
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
         gen-date generate-random-datetime,
         gen-amount gen/nat}}]
  (gen/hash-map :_id gen-id,
                :habit_id gen-habit-id,
                :date gen-date,
                :amount gen-amount))

(defn random-habit-goal-fragment-with-given-dates
  "Creates a habit goal fragment with specified `start-date` and `end-date` but at random times, and random other fields."
  [start-date end-date]
  (gen/generate (gen/let [start-date (generate-random-datetime-on-given-date start-date),
                          end-date (generate-random-datetime-on-given-date end-date),
                          total-done gen/nat,
                          successful gen/boolean]
                  {:start-date start-date, :end-date end-date, :total-done total-done, :successful successful})))

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
                        dt generate-random-monday-datetime]
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
                        dt generate-random-datetime]
           (= (:week freq) (freq-stats-util/get-habit-goal-amount-for-datetime dt freq))))

(defspec get-habit-goal-amount-for-datetime-every-x-days-frequency-test
         number-of-test-check-iterations
         (prop/for-all [freq generate-random-every-x-days-frequency,
                        dt generate-random-datetime]
           (= (:times freq) (freq-stats-util/get-habit-goal-amount-for-datetime dt freq))))

(defspec get-habit-start-date-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        total-week-frequency generate-random-total-week-frequency,
                        every-x-days-frequency generate-random-every-x-days-frequency,
                        monday-dt generate-random-monday-datetime,
                        days-to-add (gen/choose 0 6)]
           (let [later-in-week-dt (t/plus monday-dt (t/days days-to-add)),
                 sorted-habit-data [(gen/generate (generate-random-habit-day-record {:gen-date (gen/return later-in-week-dt)}))]]
             (and (= monday-dt (freq-stats-util/get-habit-start-date sorted-habit-data total-week-frequency))
                  (= later-in-week-dt (freq-stats-util/get-habit-start-date sorted-habit-data specific-day-of-week-frequency)
                        (freq-stats-util/get-habit-start-date sorted-habit-data every-x-days-frequency))))))

(defspec partition-datetimes-based-on-habit-goal-count-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        total-week-frequency generate-random-total-week-frequency,
                        every-x-days-frequency generate-random-every-x-days-frequency,
                        {:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (let [total-span (inc days-apart)]
             (and (= total-span (count (freq-stats-util/partition-datetimes-based-on-habit-goal specific-day-of-week-frequency from-date until-date)))
                  (== (Math/ceil (/ total-span 7))
                      (count (freq-stats-util/partition-datetimes-based-on-habit-goal total-week-frequency from-date until-date)))
                  (== (Math/ceil (/ total-span (:days every-x-days-frequency)))
                      (count (freq-stats-util/partition-datetimes-based-on-habit-goal every-x-days-frequency from-date until-date)))))))

(defspec partition-datetimes-based-on-habit-goal-specific-day-of-week-frequency-test
         number-of-test-check-iterations
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        {:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (let [from-date-at-start-of-day (t/with-time-at-start-of-day from-date)]
             (= (map #(-> from-date-at-start-of-day
                          (t/plus (t/days %))
                          vector)
                     (range (inc days-apart)))
                (freq-stats-util/partition-datetimes-based-on-habit-goal specific-day-of-week-frequency from-date until-date)))))

(defspec create-habit-goal-fragment-test
         number-of-test-check-iterations
         (prop/for-all [[start-date end-date :as datetimes] generate-two-random-sorted-datetimes
                        single-date generate-random-datetime]
           (and (= {:start-date start-date, :end-date end-date, :total-done 0, :successful false}
                   (freq-stats-util/create-habit-goal-fragment datetimes))
                (= {:start-date single-date, :end-date single-date, :total-done 0, :successful false}
                   (freq-stats-util/create-habit-goal-fragment [single-date])))))

(defspec span-of-habit-goal-fragment-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (let [habit-goal-fragment (random-habit-goal-fragment-with-given-dates from-date until-date)]
             (= (inc days-apart)
                (freq-stats-util/span-of-habit-goal-fragment habit-goal-fragment)))))

(defspec during-habit-goal-fragment?-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart,
                        days-to-add gen/int]
           (let [habit-goal-fragment (random-habit-goal-fragment-with-given-dates from-date until-date),
                 datetime (t/plus (gen/generate (generate-random-datetime-on-given-date from-date))
                                  (t/days days-to-add))]
             (= (<= 0 days-to-add days-apart)
                (freq-stats-util/during-habit-goal-fragment? datetime habit-goal-fragment)))))

(defspec get-habit-data-during-fragment-test
         number-of-test-check-iterations
         (prop/for-all [[from-date until-date] generate-two-random-sorted-datetimes,
                        days-to-subtract gen/s-pos-int,
                        days-to-add gen/s-pos-int]
           (let [habit-goal-fragment (random-habit-goal-fragment-with-given-dates from-date until-date),
                 too-early-dt (t/minus from-date (t/days days-to-subtract)),
                 outside-range-record-a (gen/generate (generate-random-habit-day-record {:gen-date (gen/return too-early-dt)})),
                 within-range-record-a (gen/generate (generate-random-habit-day-record {:gen-date (gen/return from-date)})),
                 within-range-record-b (gen/generate (generate-random-habit-day-record {:gen-date (gen/return until-date)})),
                 too-late-dt (t/plus until-date (t/days days-to-add)),
                 outside-range-record-b (gen/generate (generate-random-habit-day-record {:gen-date (gen/return too-late-dt)})),
                 habit-data [outside-range-record-a,
                             within-range-record-a,
                             within-range-record-b,
                             outside-range-record-b]]
             (= [within-range-record-a, within-range-record-b]
                (freq-stats-util/get-habit-data-during-fragment habit-data habit-goal-fragment)))))
