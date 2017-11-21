(ns api.db
  "Interaction with the database happens in this namespace"
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.joda-time])
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
  (let [ habit_with_id (if (contains? habit :id)
                         habit
                         (assoc habit :id (ObjectId.)))]
    (mc/insert-and-return db (:habits collection-names) habit_with_id)))

(defn get-habits
  "Retrieves all habits sync from the database as clojure maps."
  []
  (mc/find-maps db (:habits collection-names)))

(defn set-habit-data
  "Set the `amount` for a habit on a specfic day."
  [habit_id amount date-time]
  (mc/find-and-modify
             db
             (:habit_data collection-names)
             {:date date-time, :habit_id habit_id}
             {$set {:amount amount}
              $setOnInsert {:date date-time, :habit_id habit_id, :_id (ObjectId.)}}
             {:upsert true, :return-new true}))
