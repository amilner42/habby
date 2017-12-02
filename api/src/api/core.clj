(ns api.core
  (:require [api.schema :as s]
            [com.walmartlabs.lacinia.pedestal :as lp]
            [io.pedestal.http :as http])
  (:gen-class))


(def schema (s/load-schema))

(defn -main
  "Start the API."
  [& args]
  (-> schema
       (lp/service-map {:graphiql false})
       (merge { ::http/allowed-origins {:creds true :allowed-origins (constantly true)}})
       http/create-server
       http/start))
