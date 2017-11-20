(ns api.db
  "Interaction with the database happens in this namespace"
  (:require [monger.core :as mg]
            [monger.collection :as mc])
  (:import org.bson.types.ObjectId))

(def collection-names
  "The names of all the collections in the database."
  {:habits "habits"})

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
