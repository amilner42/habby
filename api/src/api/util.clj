(ns api.util
  "A namespace for holding general utilities."
  (:require [clj-time.core :as t]))


(defn date-to-y-m-d-map
  "Converts a joda date-time to a year/month/day map based on the time in UTC."
  [date-time]
  {:year (t/year date-time), :month (t/month date-time), :day (t/day date-time)})

(defn date-from-y-m-d-map
  "Converts a year/month/day map into a joda UTC date-time."
  [{:keys [year month day]}]
  (t/date-time year month day))

(defn are-datetimes-same-date
  "Input: two clj-time.core/date-time objects
  Output: true iff they are on the same date (i.e. ignore time)"
  [dt1 dt2]
  (and (= (t/year dt1) (t/year dt2))
       (= (t/month dt1) (t/month dt2))
       (= (t/day dt1) (t/day dt2))))

(defn value-map
  "Maps a function on the values of a map, returns a map with the updated values."
  [m f]
  (into {} (for [[k v] m] [k (f v)])))
