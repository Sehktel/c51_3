(defproject c51cc "0.1.0-SNAPSHOT"
  :description "C51 Clojure Compiller"
  :url "http://github.com/Sehktel/c51cc"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot c51cc.core
  :test-paths ["test"]

  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
