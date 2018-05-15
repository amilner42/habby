(ns api.dt-util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :refer [deftest, testing, is]]
            [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime,
                                 days-spanned-between-datetimes, get-consecutive-datetimes]]
            [clj-time.core :as t]))

(def generate-random-hour (gen/choose 0 23))
(def generate-random-minute (gen/choose 0 59))
(def generate-random-datetime
  (gen/let [hour generate-random-hour,
            minute generate-random-minute,
            days-to-add gen/int]
    (t/plus (t/today-at hour minute)
            (t/days days-to-add))))

(def generate-random-monday-datetime
  (gen/let [weeks-to-add gen/int]
    (t/plus (t/date-time 2018 5 7)  ; May 7 2018 was a Monday
            (t/weeks weeks-to-add))))

(defn get-later-datetime
  "Returns a `DateTime` instance `days-to-add` days later than `dt`, at time `hour`:`minute`."
  [dt days-to-add hour minute]
  (-> dt
      t/with-time-at-start-of-day
      (t/plus (t/days days-to-add)
              (t/hours hour)
              (t/minutes minute))))

; Generates a sorted two-element vector of DateTimes
(def generate-two-random-sorted-datetimes
  (gen/let [dt-a generate-random-datetime
            dt-b generate-random-datetime]
    (sort [dt-a dt-b])))

; Generates two random `DateTime`s, with `until-date` being `days-apart` days later than `from-date`. Returns a map of the values.
(def generate-two-random-datetimes-with-days-apart
  (gen/let [from-date generate-random-datetime,
            days-apart gen/nat,
            until-date-hour generate-random-hour,
            until-date-minute generate-random-minute]
    (let [until-date (get-later-datetime from-date days-apart until-date-hour until-date-minute)]
      {:from-date from-date, :until-date until-date, :days-apart days-apart})))

(defspec date-geq?-and-date-leq?-equal-dates-test
         10
         (prop/for-all [dt generate-random-datetime]
           (and (date-geq? dt dt)
                (date-leq? dt dt))))

(defspec date-geq?-test
         50
         (prop/for-all [[dt-a dt-b] generate-two-random-sorted-datetimes]
           (date-geq? dt-b dt-a)))

(defspec date-leq?-test
         50
         (prop/for-all [[dt-a dt-b] generate-two-random-sorted-datetimes]
           (date-leq? dt-a dt-b)))

(defspec first-monday-before-datetime-test
         50
         (prop/for-all [monday-dt generate-random-monday-datetime
                        days-to-add (gen/choose 0 6)]
           (let [later-in-week-dt (t/plus monday-dt (t/days days-to-add))]
             (= monday-dt (first-monday-before-datetime later-in-week-dt)))))

(deftest get-consecutive-datetimes-test
         (testing "Today until today"
                  (let [dt-a (t/today-at 3 3)
                        dt-b (t/today-at 2 2)]
                    (is (= [(t/today-at 0 0)] (get-consecutive-datetimes dt-a dt-b)))))
         (testing "Today until tomorrow"
                  (let [dt-a (t/today-at 3 3)
                        dt-b (get-later-datetime dt-a 1 2 2)]
                    (is (= [(t/today-at 0 0), (t/plus (t/today-at 0 0) (t/days 1))]
                           (get-consecutive-datetimes dt-a dt-b))))))

(defspec get-consecutive-datetimes-count-test
         10
         (prop/for-all [{:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (= (inc days-apart)
              (count (get-consecutive-datetimes from-date until-date)))))

(defspec days-spanned-between-datetimes-test
         50
         (prop/for-all [{:keys [from-date until-date days-apart]} generate-two-random-datetimes-with-days-apart]
           (= (inc days-apart)
              (days-spanned-between-datetimes from-date until-date))))
