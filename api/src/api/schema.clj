(ns api.schema
  "Contains custom resolvers and a function to provide the full schema."
  (:require [api.db :as db]
            [clojure.java.io :as io]
            [com.walmartlabs.lacinia.util :as util]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.resolve :as resolve]
            [clojure.core.async :refer [thread]]
            [clojure.edn :as edn]
            [clj-time.core :as t])
  (:use [slingshot.slingshot :only [throw+, try+]]))


(defn date-to-y-m-d-map
  "Converts a joda date-time to a year/month/day map based on the time in UTC."
  [date-time]
  {:year (t/year date-time), :month (t/month date-time), :day (t/day date-time)})

(defn value-map
  "Maps a function on the values of a map, returns a map with the updated values."
  [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn unnest-tagged-unions-on-input-object
  "Recursively converts all the tagged unions in the input object to have data unnested.
  Will look for the value ascociated with the`:type_name` key and unnest the data stored
  with that as its key, if no :type_name key is present or it isn't a map, returns
  the input-object unchanged. Does this recursively for all nested keys if it is a map."
  [input-object]
  (let [type_name (and (map? input-object) (:type_name input-object))]
    (if (and (map? input-object) (not (nil? type_name)))
      (as-> (input-object (keyword type_name)) rtrn
        (if (nil? rtrn) (throw+ {:type ::invalid_tagged_union_null :tag_name type_name}) rtrn)
        (value-map rtrn unnest-tagged-unions-on-input-object)
        (assoc rtrn :type_name type_name))
      input-object)))

(defn tag-type
  "Tag a map using `schema/tag-with-type` and the map's `type_name`."
  [map-with-type]
  (schema/tag-with-type map-with-type (keyword (:type_name map-with-type))))

(defn tag-type-recursive
  "Tag a map using `tag-type` and recurse on its children."
  [maybe-mp]
  (let [type_name (and (map? maybe-mp) (:type_name maybe-mp))]
    (if (map? maybe-mp)
      (if (nil? type_name)
        (value-map maybe-mp tag-type-recursive)
        (tag-type (value-map maybe-mp tag-type-recursive)))
      maybe-mp)))

(defn create-async-resolver
  "Spawn a thread to run the resolver and immediately return a lacinia `ResolverResultPromise`."
  [resolver]
  (fn [context args value]
    (let [result (resolve/resolve-promise)]
      (thread
       (try+
         (resolve/deliver! result (resolver context args value))
         (catch [:type ::invalid_tagged_union_null] {:keys [tag_name]}
           (resolve/deliver! result nil
                             {:message (str
                                        "Invalid tagged union, tag name was "
                                        tag_name
                                        " but value associated with that key was null.")}))
         (catch Throwable t
           (resolve/deliver! result nil
                             {:message (str "Exception: " (.getMessage t))}))))

      result)))

(defn create-tag-type-resolver
  "Create a resolver which returns the `field-name` of `value` tagged with `tag-type`."
  [field-name]
  (fn [context args value] (tag-type (value field-name))))

(defn resolve-get-habits
  "Get all the habits from the database."
  [context args value]
  (map tag-type (db/get-habits)))

(defn resolve-mutation-add-habit
  "Add a habit to the database and get the habit back."
  [context {:keys [create_habit_data] } value]
  (tag-type-recursive (db/add-habit (unnest-tagged-unions-on-input-object create_habit_data))))

(defn resolve-mutation-set-habit-data
  "Add some new habit data to the database."
  [context {:keys [habit_id amount date]} value]
  (let [date-time (t/date-time (:year date) (:month date) (:day date))]
    (update (db/set-habit-data habit_id amount date-time) :date date-to-y-m-d-map)))

(defn resolver-map
  []
  {:query/get-habits (create-async-resolver resolve-get-habits)
   :query/tag-type-for-threshold-frequency (create-tag-type-resolver :threshold_frequency)
   :query/tag-type-for-target-frequency (create-tag-type-resolver :target_frequency)
   :query/resolve-mutation-add-habit (create-async-resolver resolve-mutation-add-habit)
   :query/resolve-mutation-set-habit-data (create-async-resolver resolve-mutation-set-habit-data)})

; We load our EDN schema file and attach our resolvers from our resolver map
; before compiling. We must do the attachment of the resolvers before compiling.
(defn load-schema
  []
  (-> (io/resource "schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers (resolver-map))
      schema/compile))
