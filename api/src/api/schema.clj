(ns api.schema
  "Contains custom resolvers and a function to provide the full schema."
  (:require
    [api.db :as db]
    [clojure.java.io :as io]
    [com.walmartlabs.lacinia.util :as util]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.lacinia.resolve :as resolve]
    [clojure.core.async :refer [thread]]
    [clojure.edn :as edn]))


(defn tag-type
  "Tag a map using `schema/tag-with-type` and the map's `type_name`."
  [ map-with-type ]
  (schema/tag-with-type map-with-type (keyword (:type_name map-with-type))))

(defn create-async-resolver
  "Spawn a thread to run the resolver and immediately return a lacinia `ResolverResultPromise`."
  [ resolver ]
  (fn [context args value]
    (let [result (resolve/resolve-promise)]
      (thread
       (try
         (resolve/deliver! result (resolver context args value))
         (catch Throwable t
           (resolve/deliver! result nil
                             {:message (str "Exception: " (.getMessage t))}))))

      result)))

(defn create-tag-type-resolver
  "Create a resolver which returns `field-name` of `value` tagged."
  [ field-name ]
  (fn [context args value] (tag-type (value (keyword field-name)))))

(defn resolve-get-habits
  [context args value]
  (map tag-type (db/get-habits)))

(defn resolver-map
  []
  {:query/get-habits (create-async-resolver resolve-get-habits)
   :query/tag-type-for-threshold-frequency (create-tag-type-resolver "threshold_frequency")
   :query/tag-type-for-target-frequency (create-tag-type-resolver "target_frequency")})

; We load our EDN schema file and attach our resolvers from our resolver map
; before compiling. We must do the attachment of the resolvers before compiling.
(defn load-schema
  []
  (-> (io/resource "schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers (resolver-map))
      schema/compile))
