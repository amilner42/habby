(ns api.dt-util
  "A namespace for holding utilities related to `DateTime`s."
  (:require [clj-time.core :as t]
            [clj-time.predicates :refer [same-date?]]
            [clj-time.periodic :refer [periodic-seq]]))

(defn date-geq?
  "Returns true iff `dt1` falls on the same date or later as `dt2`.
  Ignores the time."
  [dt1 dt2]
  (or (same-date? dt1 dt2)
      (t/after? dt1 dt2)))

(defn date-leq?
  "Returns true iff `dt1` falls on the same date or earlier as `dt2`.
  Ignores the time."
  [dt1 dt2]
  (or (same-date? dt1 dt2)
      (t/before? dt1 dt2)))

(defn first-monday-before-datetime
  "Returns the date of the first Monday before `dt`.
  Subtracting (<day of week of `dt`> - 1) days from `dt` yields the most recent Monday.
  Note that clj-time numbers the days of the week with 1 for Monday and 7 for Sunday."
  [dt]
  (t/minus dt
           (-> dt
               t/day-of-week
               dec
               t/days)))

(defn get-consecutive-datetimes
  "Returns a list of consecutive datetimes from `from-dt` to `until-dt`, inclusive.
  All datetimes are generated at the start of the day so that times are ignored.
  `from-dt` should be an earlier or equal date to `until-dt`."
  [from-dt until-dt]
  (let [from-dt (t/with-time-at-start-of-day from-dt)
        until-dt (t/with-time-at-start-of-day until-dt)]
    (periodic-seq from-dt
                  (t/plus until-dt (t/days 1))
                  (t/days 1))))

(defn day-of-week-keyword
  "Returns the day of week of a `DateTime`, in keyword form."
  [dt]
  (condp = (t/day-of-week dt)
         1 :monday, 2 :tuesday, 3 :wednesday, 4 :thursday, 5 :friday, 6 :saturday, 7 :sunday))

(defn days-spanned-between-datetimes
  "Returns the number of days spanned from `from-dt` to `until-dt`, inclusive.
  For instance, today until tomorrow would be 2 days spanned.
  `from-dt` should be an earlier or equal date to `until-dt`."
  [from-dt until-dt]
  (-> from-dt
      t/with-time-at-start-of-day  ; Bring `from-dt` to start of day so that times don't interfere
      (t/interval until-dt)
      t/in-days
      inc))
