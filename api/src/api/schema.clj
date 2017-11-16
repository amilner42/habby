(ns api.schema
  "Contains custom resolvers and a function to provide the full schema."
  (:require
    [clojure.java.io :as io]
    [com.walmartlabs.lacinia.util :as util]
    [com.walmartlabs.lacinia.schema :as schema]
    [clojure.edn :as edn]))

(defn resolve-get-habits
  []
  "TODO")

(defn resolver-map
  []
  {:query/get-habits resolve-get-habits})

; We load our EDN schema file and attach our resolvers from our resolver map
; before compiling. We must do the attachment of the resolvers before compiling.
(defn load-schema
  []
  (-> (io/resource "schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers (resolver-map))
      schema/compile))
