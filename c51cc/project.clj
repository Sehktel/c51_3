(defproject c51cc "0.1.0-SNAPSHOT"
  :description "C51 Clojure Compiller"
  :url "http://github.com/Sehktel/c51cc"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.10"]]
  
  ;; Настройки для тестирования
  :test-paths ["test"]
  :source-paths ["src"]
  
  ;; Профили для разработки
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "1.3.0"]
                                  [org.clojure/test.check "1.1.1"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user}}
             :test {:resource-paths ["test-resources"]}}
  
  ;; Плагины
  :plugins [[lein-cloverage "1.2.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [com.github.clj-kondo/lein-clj-kondo "0.2.5"]
            ]
  
  :bikeshed {
    :max-line-length 125
    :trailing-whitespace true
    :var-redefs false
    :name-collisions false
  }
  
  ;; Настройки JVM
  :jvm-opts ["-Xmx1g"]
  
  ;; Точка входа для запуска
  :main c51cc.core)
