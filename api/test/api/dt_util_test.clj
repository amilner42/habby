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

(def gen-hour (gen/choose 0 23))

(def gen-minute (gen/choose 0 59))

(def gen-datetime
  (gen/let [hour gen-hour,
            minute gen-minute,
            num-days-to-add gen/int]
    (t/plus (t/today-at hour minute)
            (t/days num-days-to-add))))

(def gen-monday-datetime
  (gen/let [num-weeks-to-add gen/int]
    (t/plus (t/date-time 2018 5 7)  ; May 7 2018 was a Monday
            (t/weeks num-weeks-to-add))))

(defn gen-datetime-on-given-date
  "Returns a generator for a `DateTime` instance on the same date as `dt`, with a random time."
  [dt]
  (gen/let [hour gen-hour,
            minute gen-minute]
    (-> dt
        t/with-time-at-start-of-day
        (t/plus (t/hours hour) (t/minutes minute)))))

(defn random-datetime-on-given-date
  "Like `gen-datetime-on-given-date`, but returns a `DateTime` instead of a generator for one."
  [dt]
  (gen/generate (gen-datetime-on-given-date dt)))

; Generates a sorted two-element vector of DateTimes
(def gen-two-sorted-datetimes
  (gen/let [dt-a gen-datetime
            dt-b gen-datetime]
    (sort [dt-a dt-b])))

; Generates two random `DateTime`s, with `until-date` being `num-days-apart` days later than `from-date`. Returns a map of the values.
(def gen-two-datetimes-with-num-days-apart
  (gen/let [from-date gen-datetime,
            num-days-apart gen/nat]
    (let [until-date (t/plus (random-datetime-on-given-date from-date)
                             (t/days num-days-apart))]
      {:from-date from-date, :until-date until-date, :num-days-apart num-days-apart})))

;; Tests
;; ---------------------------------------------------------------------------

(defspec date-geq?-and-date-leq?-equal-dates-test
         number-of-test-check-iterations
         (prop/for-all [dt gen-datetime]
           (and (date-geq? dt dt)
                (date-leq? dt dt))))

(defspec date-geq?-true-test
         number-of-test-check-iterations
         (prop/for-all [[dt-a dt-b] gen-two-sorted-datetimes]
           (date-geq? dt-b dt-a)))

(defspec date-geq?-false-test
         number-of-test-check-iterations
         (prop/for-all [dt-a gen-datetime
                        num-days-later gen/s-pos-int]
           (let [dt-b (random-datetime-on-given-date (t/plus dt-a (t/days num-days-later)))]
             (not (date-geq? dt-a dt-b)))))

(defspec date-leq?-true-test
         number-of-test-check-iterations
         (prop/for-all [[dt-a dt-b] gen-two-sorted-datetimes]
           (date-leq? dt-a dt-b)))

(defspec date-leq?-false-test
         number-of-test-check-iterations
         (prop/for-all [dt-a gen-datetime
                        num-days-later gen/s-pos-int]
           (let [dt-b (random-datetime-on-given-date (t/plus dt-a (t/days num-days-later)))]
             (not (date-leq? dt-b dt-a)))))

(defspec first-monday-before-datetime-test
         number-of-test-check-iterations
         (prop/for-all [monday-dt gen-monday-datetime
                        num-days-later (gen/choose 0 6)]
           (let [later-in-week-dt (t/plus monday-dt (t/days num-days-later))]
             (= monday-dt (first-monday-before-datetime later-in-week-dt)))))

(defspec get-consecutive-datetimes-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} gen-two-datetimes-with-num-days-apart]
             (let [from-date-at-start-of-day (t/with-time-at-start-of-day from-date)]
               (= (map #(t/plus from-date-at-start-of-day (t/days %)) (range (inc num-days-apart)))
                  (get-consecutive-datetimes from-date until-date)))))

; Test that each `DateTime` produced by `get-consecutive-datetimes` is at the start of the day
(defspec get-consecutive-datetimes-start-of-day-test
         number-of-test-check-iterations
         (prop/for-all [[dt-a dt-b] gen-two-sorted-datetimes]
           (every? #(= 0 (t/hour %) (t/minute %))
                   (get-consecutive-datetimes dt-a dt-b))))

; Test that `get-consecutive-datetimes` produces the correct number of `DateTime`s
(defspec get-consecutive-datetimes-count-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} gen-two-datetimes-with-num-days-apart]
           (= (inc num-days-apart)
              (count (get-consecutive-datetimes from-date until-date)))))

(defspec days-spanned-between-datetimes-test
         number-of-test-check-iterations
         (prop/for-all [{:keys [from-date until-date num-days-apart]} gen-two-datetimes-with-num-days-apart]
           (= (inc num-days-apart)
              (days-spanned-between-datetimes from-date until-date))))
