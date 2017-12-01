(defproject api "0.1.0-SNAPSHOT"
  :description "The API backing habby clients."
  :url "http://example.com/FIXME"
  :license {:name "GPL-3.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.walmartlabs/lacinia "0.21.0"]
                 [com.walmartlabs/lacinia-pedestal "0.5.0-rc-2"]
                 [io.aviso/logging "0.2.0"]
                 [com.novemberain/monger "3.1.0"]
                 [slingshot "0.12.2"]
                 [clj-time "0.14.2"]
                 [proto-repl "0.3.1"]]
  :main ^:skip-aot api.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
