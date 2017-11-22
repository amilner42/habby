(ns api.db
  "Interaction with the database happens in this namespace"
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.joda-time]
            [api.util :refer [date-to-y-m-d-map, date-from-y-m-d-map]])
  (:import org.bson.types.ObjectId org.joda.time.DateTimeZone))

(DateTimeZone/setDefault DateTimeZone/UTC)

(def collection-names
  "The names of all the collections in the database."
  {:habits "habits"
   :habit_data "habit_data"})

(defonce connection (mg/connect))

(defonce db (mg/get-db connection "habby"))

(defn add-habit
  "Add a habit to the database and returns that habit including the ID.
  Will create an ID if the habit passed doesn't have an ID."
  [habit]
  (let [ habit_with_id (if (contains? habit :_id)
                         habit
                         (assoc habit :_id (ObjectId.)))]
    (mc/insert-and-return db (:habits collection-names) habit_with_id)))

(defn delete-habit
  "Deletes a habit from the database, returns true if the habit was deleted."
  [habit_id]
  (= 1 (.getN (mc/remove-by-id db (:habits collection-names) (ObjectId. habit_id)))))

(defn get-habits
  "Retrieves all habits sync from the database as clojure maps."
  []
  (mc/find-maps db (:habits collection-names)))

(defn get-habit-data
  "Gets habit data from the db, optionally after a specific date or for a specific habit."
  [after_date for_habit]
  (as-> {} find-query-filter
        (if (nil? for_habit) find-query-filter (assoc find-query-filter :habit_id (ObjectId. for_habit)))
        (if (nil? after_date) find-query-filter (assoc find-query-filter :date {$gte (date-from-y-m-d-map after_date)}))
        (mc/find-maps db (:habit_data collection-names) find-query-filter )))

(defn set-habit-data
  "Set the `amount` for a habit on a specfic day."
  [habit_id amount date-time]
  (mc/find-and-modify
             db
             (:habit_data collection-names)
             {:date date-time, :habit_id (ObjectId. habit_id)}
             {$set {:amount amount}
              $setOnInsert {:date date-time, :habit_id (ObjectId. habit_id), :_id (ObjectId.)}}
             {:upsert true, :return-new true}))
