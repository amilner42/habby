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

(defn resolve-asynchronously
  "Spawn a thread to run the resolver and immedietely return a lacinia `ResolverResultPromise`."
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

(defn resolve-get-habits
  [context args value]
  (db/get-habits))


(defn resolver-map
  []
  {:query/get-habits (resolve-asynchronously resolve-get-habits)})

; We load our EDN schema file and attach our resolvers from our resolver map
; before compiling. We must do the attachment of the resolvers before compiling.
(defn load-schema
  []
  (-> (io/resource "schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers (resolver-map))
      schema/compile))
