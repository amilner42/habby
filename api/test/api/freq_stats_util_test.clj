(ns api.freq-stats-util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clj-time.core :as t]
            [api.dt-util :refer [days-spanned-between-datetimes]]
            [api.freq-stats-util :refer [get-habit-goal-fragment-length, get-habit-goal-amount-for-datetime,
                                         get-habit-start-date, partition-datetimes-based-on-habit-goal, create-habit-goal-fragment,
                                         span-of-habit-goal-fragment, during-habit-goal-fragment?]]
            [api.dt-util-test :refer [generate-random-datetime, generate-random-monday-datetime, generate-two-random-sorted-datetimes,
                                      generate-two-random-datetimes-with-days-apart, generate-random-hour, generate-random-minute,
                                      get-later-datetime]])
  (:import org.bson.types.ObjectId))

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

(def generate-random-habit-day-record
  (gen/hash-map :_id generate-random-ID,
                :habit_id generate-random-ID,
                :date generate-random-datetime,
                :amount gen/nat))

(defn random-habit-goal-fragment-with-given-dates
  "Creates a habit goal fragment with specified `start-date` and `end-date`, and random `total-done` and `successful` fields."
  [start-date end-date]
  (gen/generate (gen/let [total-done gen/nat, successful gen/boolean]
                  {:start-date start-date, :end-date end-date, :total-done total-done, :successful successful})))

(defspec get-habit-goal-fragment-length-specific-day-of-week-frequency-test
         10
         (prop/for-all [freq generate-random-specific-day-of-week-frequency]
           (= 1 (get-habit-goal-fragment-length freq))))

(defspec get-habit-goal-fragment-length-total-week-frequency-test
         10
         (prop/for-all [freq generate-random-total-week-frequency]
           (= 7 (get-habit-goal-fragment-length freq))))

(defspec get-habit-goal-fragment-length-every-x-days-frequency-test
         10
         (prop/for-all [freq generate-random-every-x-days-frequency]
           (= (:days freq) (get-habit-goal-fragment-length freq))))

(defspec get-habit-goal-amount-for-datetime-specific-day-of-week-frequency-test
         10
         (prop/for-all [freq generate-random-specific-day-of-week-frequency,
                        dt generate-random-monday-datetime]
           (and (= (:monday freq) (get-habit-goal-amount-for-datetime dt freq))
                (= (:tuesday freq) (get-habit-goal-amount-for-datetime (t/plus dt (t/days 1)) freq))
                (= (:wednesday freq) (get-habit-goal-amount-for-datetime (t/plus dt (t/days 2)) freq))
                (= (:thursday freq) (get-habit-goal-amount-for-datetime (t/plus dt (t/days 3)) freq))
                (= (:friday freq) (get-habit-goal-amount-for-datetime (t/plus dt (t/days 4)) freq))
                (= (:saturday freq) (get-habit-goal-amount-for-datetime (t/plus dt (t/days 5)) freq))
                (= (:sunday freq) (get-habit-goal-amount-for-datetime (t/plus dt (t/days 6)) freq)))))

(defspec get-habit-goal-amount-for-datetime-total-week-frequency-test
         10
         (prop/for-all [freq generate-random-total-week-frequency,
                        dt generate-random-datetime]
           (= (:week freq) (get-habit-goal-amount-for-datetime dt freq))))

(defspec get-habit-goal-amount-for-datetime-every-x-days-frequency-test
         10
         (prop/for-all [freq generate-random-every-x-days-frequency,
                        dt generate-random-datetime]
           (= (:times freq) (get-habit-goal-amount-for-datetime dt freq))))

(defspec get-habit-start-date-test
         50
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        total-week-frequency generate-random-total-week-frequency,
                        every-x-days-frequency generate-random-every-x-days-frequency,
                        habit-day-record generate-random-habit-day-record,
                        monday-dt generate-random-monday-datetime,
                        days-to-add (gen/choose 0 6)]
           (let [later-in-week-dt (t/plus monday-dt (t/days days-to-add)),
                 sorted-habit-data [(assoc habit-day-record :date later-in-week-dt)]]
             (and (= monday-dt (get-habit-start-date sorted-habit-data total-week-frequency))
                  (= later-in-week-dt (get-habit-start-date sorted-habit-data specific-day-of-week-frequency)
                        (get-habit-start-date sorted-habit-data every-x-days-frequency))))))

(defspec partition-datetimes-based-on-habit-goal-count-test
         20
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        total-week-frequency generate-random-total-week-frequency,
                        every-x-days-frequency generate-random-every-x-days-frequency,
                        [from-date until-date] generate-two-random-sorted-datetimes]
           (let [total-span (days-spanned-between-datetimes from-date until-date)]
             (and (= total-span (count (partition-datetimes-based-on-habit-goal specific-day-of-week-frequency from-date until-date)))
                  (== (Math/ceil (/ total-span 7))
                      (count (partition-datetimes-based-on-habit-goal total-week-frequency from-date until-date)))
                  (== (Math/ceil (/ total-span (:days every-x-days-frequency)))
                      (count (partition-datetimes-based-on-habit-goal every-x-days-frequency from-date until-date)))))))

(defspec partition-datetimes-based-on-habit-goal-specific-day-of-week-frequency-test
         20
         (prop/for-all [specific-day-of-week-frequency generate-random-specific-day-of-week-frequency,
                        {:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (let [from-date-at-start-of-day (t/with-time-at-start-of-day from-date)]
             (= (map #(-> from-date-at-start-of-day
                          (t/plus (t/days %))
                          vector)
                     (range (inc days-apart)))
                (partition-datetimes-based-on-habit-goal specific-day-of-week-frequency from-date until-date)))))

(defspec create-habit-goal-fragment-test
         20
         (prop/for-all [[start-date end-date :as datetimes] generate-two-random-sorted-datetimes
                        single-date generate-random-datetime]
           (and (= {:start-date start-date, :end-date end-date, :total-done 0, :successful false}
                   (create-habit-goal-fragment datetimes))
                (= {:start-date single-date, :end-date single-date, :total-done 0, :successful false}
                   (create-habit-goal-fragment [single-date])))))

(defspec span-of-habit-goal-fragment-test
         20
         (prop/for-all [{:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (let [habit-goal-fragment (random-habit-goal-fragment-with-given-dates from-date until-date)]
             (= (inc days-apart)
                (span-of-habit-goal-fragment habit-goal-fragment)))))

(defspec during-habit-goal-fragment?-test
         20
         (prop/for-all [{:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart,
                        days-to-add gen/int,
                        datetime-hour generate-random-hour,
                        datetime-minute generate-random-minute]
           (let [habit-goal-fragment (random-habit-goal-fragment-with-given-dates from-date until-date),
                 datetime (get-later-datetime from-date days-to-add datetime-hour datetime-minute)]
             (= (<= 0 days-to-add days-apart)
                (during-habit-goal-fragment? datetime habit-goal-fragment)))))
