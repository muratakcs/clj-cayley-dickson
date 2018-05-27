(defproject clj-cayley-dickson "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-ancient "0.6.14"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [me.raynes/fs "1.4.6"]
                 [net.mikera/imagez "0.12.0"]
                 [image-resizer "0.1.10"]]
  :main ^:skip-aot og.clj-cayley-dickson.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
