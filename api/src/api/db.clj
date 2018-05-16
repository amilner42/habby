(ns api.db
  "Interaction with the database happens in this namespace"
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.joda-time]
            [api.util :refer :all]
            [api.freq-stats-util :refer [get-freq-stats-for-habit]]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId org.joda.time.DateTimeZone))

(DateTimeZone/setDefault DateTimeZone/UTC)

(def collection-names
  "The names of all the collections in the database."
  {:habits "habits"
   :habit_data "habit_data"})

(defonce connection (mg/connect))

(defonce habby_db (mg/get-db connection "habby"))

(defn add-habit
  "Add a habit to the database and returns that habit including the ID.
  Will create an ID if the habit passed doesn't have an ID. Will set `suspended` to false."
  [{:keys [db habit] :or {db habby_db}}]
  (let [ final_habit (as-> habit habit
                          (if (contains? habit :_id) habit (assoc habit :_id (ObjectId.)))
                          (assoc habit :suspended false))]
    (mc/insert-and-return db (:habits collection-names) final_habit)))

(defn delete-habit
  "Deletes a habit from the database, returns true if the habit was deleted."
  [{:keys [db habit_id] :or {db habby_db}}]
  (= 1 (.getN (mc/remove-by-id db (:habits collection-names) (ObjectId. habit_id)))))

(defn set-suspend-habit
  "Set the `suspended` for a habit, returns true if the update was performed on the habit."
  [{:keys [db habit_id suspended] :or {db habby_db}}]
  (= 1 (.getN (mc/update db (:habits collection-names) {:_id (ObjectId. habit_id)} {$set {:suspended suspended}}))))

(defn get-habits
  "Retrieves all habits sync from the database as clojure maps."
  [{:keys [db habit_ids] :or {db habby_db}}]
  (mc/find-maps db
                (:habits collection-names)
                (if (nil? habit_ids) nil {:_id {$in (map #(ObjectId. %) habit_ids)}})))

(defn get-habit-data
  "Gets habit data from the db, optionally after/before a specific date or for specific habits."
  [{:keys [db after_date before_date habit_ids] :or {db habby_db}}]
  (as-> {} find-query-filter
        (if (nil? habit_ids) find-query-filter (assoc find-query-filter :habit_id {$in (map #(ObjectId. %) habit_ids)}))
        (if (nil? after_date) find-query-filter (assoc find-query-filter :date {$gte (date-from-y-m-d-map after_date)}))
        (if (nil? before_date)
          find-query-filter
          (assoc find-query-filter
                 ; Require dates be earlier than midnight the day after `before_date`, so that times don't interfere
                 :date {$lt (t/plus (date-from-y-m-d-map before_date) (t/days 1))}))
        (mc/find-maps db (:habit_data collection-names) find-query-filter)))

(defn set-habit-data
  "Set the `amount` for a habit on a specfic day."
  [{:keys [db habit_id amount date-time] :or {db habby_db}}]
  (mc/find-and-modify db
                      (:habit_data collection-names)
                      {:date date-time, :habit_id (ObjectId. habit_id)}
                      {$set {:amount amount}
                       $setOnInsert {:date date-time, :habit_id (ObjectId. habit_id), :_id (ObjectId.)}}
                      {:upsert true, :return-new true}))

(defn get-frequency-stats
  "Returns performance statistics for the requested habits.
  Retrieves habits and habit data from database `db`.
  Analyzes user performance based on habit data from `current_client_date` or earlier.
  Generates a list of `habit_frequency_stats` maps, one for each ID in `habit_ids`."
  [{:keys [db habit_ids current_client_date] :or {db habby_db, current_client_date (t/today-at 0 0)}}]
  (let [all-habits (get-habits {:db db,
                                :habit_ids habit_ids}),
        all-habit-data-until-current-date (get-habit-data {:db db,
                                                           :before_date (date-to-y-m-d-map current_client_date),
                                                           :habit_ids habit_ids})]
    (map #(get-freq-stats-for-habit db % all-habit-data-until-current-date current_client_date) all-habits)))
