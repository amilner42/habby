(ns api.dt-util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime,
                                 days-spanned-between-datetimes, get-consecutive-datetimes]]
            [clj-time.core :as t]))

(def number-of-test-check-iterations 30)

;; Generators
;; ---------------------------------------------------------------------------

(def generate-random-hour (gen/choose 0 23))

(def generate-random-minute (gen/choose 0 59))

(def generate-random-datetime
  (gen/let [hour generate-random-hour,
            minute generate-random-minute,
            num-days-to-add gen/int]
    (t/plus (t/today-at hour minute)
            (t/days num-days-to-add))))

(def generate-random-monday-datetime
  (gen/let [num-weeks-to-add gen/int]
    (t/plus (t/date-time 2018 5 7)  ; May 7 2018 was a Monday
            (t/weeks num-weeks-to-add))))

(defn generate-random-datetime-on-given-date
  "Returns a generator for a `DateTime` instance on the same date as `dt`, with a random time."
  [dt]
  (gen/let [hour generate-random-hour,
            minute generate-random-minute]
    (-> dt
        t/with-time-at-start-of-day
        (t/plus (t/hours hour) (t/minutes minute)))))

(defn random-datetime-on-given-date
  "Like `generate-random-datetime-on-given-date`, but returns a `DateTime` instead of a generator for one."
  [dt]
  (gen/generate (generate-random-datetime-on-given-date dt)))

; Generates a sorted two-element vector of DateTimes
(def generate-two-random-sorted-datetimes
  (gen/let [dt-a generate-random-datetime
            dt-b generate-random-datetime]
    (sort [dt-a dt-b])))

; Generates two random `DateTime`s, with `until-date` being `num-days-apart` days later than `from-date`. Returns a map of the values.
(def generate-two-random-datetimes-with-num-days-apart
  (gen/let [from-date generate-random-datetime,
            num-days-apart gen/nat]
    (let [until-date (t/plus (random-datetime-on-given-date from-date)
                             (t/days num-days-apart))]
      {:from-date from-date, :until-date until-date, :num-days-apart num-days-apart})))

;; Tests
;; ---------------------------------------------------------------------------

(defspec date-geq?-and-date-leq?-equal-dates-test
         number-of-test-check-iterations
         (prop/for-all [dt generate-random-datetime]
           (and (date-geq? dt dt)
                (date-leq? dt dt))))

(defspec date-geq?-test
         number-of-test-check-iterations
         (prop/for-all [[dt-a dt-b] generate-two-random-sorted-datetimes]
           (date-geq? dt-b dt-a)))

(defspec date-leq?-test
         number-of-test-check-iterations
         (prop/for-all [[dt-a dt-b] generate-two-random-sorted-datetimes]
           (date-leq? dt-a dt-b)))

(defspec first-monday-before-datetime-test
         number-of-test-check-iterations
         (prop/for-all [monday-dt generate-random-monday-datetime
                        num-days-later (gen/choose 0 6)]
           (let [later-in-week-dt (t/plus monday-dt (t/days num-days-later))]
             (= monday-dt (first-monday-before-datetime later-in-week-dt)))))

(defspec get-consecutive-datetimes-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} generate-two-random-datetimes-with-num-days-apart]
             (let [from-date-at-start-of-day (t/with-time-at-start-of-day from-date)]
               (= (map #(t/plus from-date-at-start-of-day (t/days %)) (range (inc num-days-apart)))
                  (get-consecutive-datetimes from-date until-date)))))

(defspec days-spanned-between-datetimes-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} generate-two-random-datetimes-with-num-days-apart]
           (= (inc num-days-apart)
              (days-spanned-between-datetimes from-date until-date))))
