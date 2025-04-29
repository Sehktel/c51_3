(defproject c51cc "0.1.0-SNAPSHOT"
  :description "C51 Clojure Compiler"
  :url "http://github.com/Sehktel/c51cc"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.10"]
                 [org.clojure/tools.logging "1.2.1"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/tools.cli "1.0.206"]]
  
  ;; Настройки для тестирования
  :test-paths ["test"]
  :source-paths ["src"]
  :resource-paths ["c51code"]
  
  ;; Профили для разработки
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "1.3.0"]
                                  [org.clojure/test.check "1.1.1"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user}}
             
             :test {:resource-paths ["c51code"]
                    :env {:file-c "c51code/simple_example.c"
                          :include-path "c51code"}}}
  
  ;; Плагины
  :plugins [[lein-cloverage "1.2.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [com.github.clj-kondo/lein-clj-kondo "0.2.5"]]
  
  :bikeshed {
    :max-line-length 125
    :trailing-whitespace true
    :var-redefs false
    :name-collisions false
  }
  
  ;; Настройки JVM
  :jvm-opts ["-Xmx2g"]
  
  ;; Точка входа для запуска
  ;;:main c51cc.core
  :main c51cc.compiler
  
  :test-selectors {
    :manual (fn [m] (:manual m))
    :manual-parse (fn [m] 
                    (and 
                      (:manual m)
                      (= (str (:name m)) "test-manual-parse")))
  })
