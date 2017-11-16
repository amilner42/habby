(ns api.db
  "Interaction with the database happens in this namespace"
  (:require
    [monger.core :as mg]
    [monger.collection :as mc])
  (:import [com.mongodb MongoOptions ServerAddress]))

(def collection-names
  "The names of all the collections in the database."
  {:habits "habits"})

(defonce connection (mg/connect))

(defonce db (mg/get-db connection "habby"))

(defn add-habit
  "Add a habit to the database and returns that habit including the ID."
  [habit]
  (mc/insert-and-return db (:habits collection-names) habit))

(defn get-habits
  "Retrieves all habits from the database."
  []
  (mc/find-maps db (:habits collection-names)))
