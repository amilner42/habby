(ns user
  "The user gets loaded by the repl by default, just helps with testing queries out..."
  (:require [api.schema :as s]
            [com.walmartlabs.lacinia :as lacinia]
            [com.walmartlabs.lacinia.pedestal :as lp]
            [io.pedestal.http :as http]
            [clojure.walk :as walk]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test :refer [run-tests]])
  (:import (clojure.lang IPersistentMap)))

(def schema (s/load-schema))

(defn simplify
  "Converts all ordered maps nested within the map into standard hash maps, and
   sequences into vectors, which makes for easier constants in the tests, and eliminates ordering problems."
  [m]
  (walk/postwalk
    (fn [node]
      (cond
        (instance? IPersistentMap node)
        (into {} node)

        (seq? node)
        (vec node)

        :else
        node))
    m))

(defn q
  [query-string]
  (-> (lacinia/execute schema query-string nil nil)
      simplify))

(defonce server nil)

(defn start-server
  [_]
  (-> schema
      (lp/service-map {:graphiql true})
      (merge { ::http/allowed-origins {:creds true :allowed-origins (constantly true)}})
      http/create-server
      http/start))

(defn stop-server
  [server]
  (http/stop server)
  nil)

(defn start
  []
  (alter-var-root #'server start-server)
  :started)

(defn stop
  []
  (alter-var-root #'server stop-server)
  :stopped)
