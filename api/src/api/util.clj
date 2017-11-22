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

(defn value-map
  "Maps a function on the values of a map, returns a map with the updated values."
  [m f]
  (into {} (for [[k v] m] [k (f v)])))
