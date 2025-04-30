(ns c51cc.ast
  "Модуль для генерации и анализа абстрактного синтаксического дерева (AST)
   
   Расширенные возможности:
   - Полный цикл обработки файлов
   - Расширенная диагностика
   - Гибкий препроцессинг и токенизация"
  (:require 
            [c51cc.logger :as log]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :as stacktrace])
  (:import (java.io File)))

(def node-types
  "Типы узлов абстрактного синтаксического дерева (AST)"
  {:program :program
   :function-declaration :function-declaration
   :variable-declaration :variable-declaration
   :function-call :function-call
   :control-flow :control-flow
   :expression :expression})

;; (defn generate-ast
  ;; "Генерация абстрактного синтаксического дерева с расширенной обработкой"
  ;; [code base-path]
  ;; (log/info "Начало генерации AST")
  ;; (let [preprocessed-code (preprocessor/preprocess code :base-path base-path)
  ;;       tokens (lexer/tokenize preprocessed-code)
  ;;       ast (parser/parse tokens)]
  ;;   (log/debug "AST успешно сгенерирован")
  ;;   (println "Токены:" tokens)
  ;;   (println "AST:" (with-out-str (clojure.pprint/pprint ast)))
  ;;   ast))

(defn pretty-print-ast
  "Расширенная визуализация AST с глубоким форматированием и анализом

   Теоретические аспекты:
   - Многоуровневая визуализация структуры
   - Ограничение глубины для читаемости
   - Семантическая аннотация узлов

   Сложность: O(n), где n - количество узлов в AST"
  [ast]
  (log/debug "Начало расширенной визуализации AST")
  (with-out-str
    (pprint/pprint 
     (let [processed-ast 
           (update-in ast [:nodes] 
                      (fn [nodes] 
                        (mapv 
                         (fn [node]
                           (let [base-node 
                                 (select-keys node 
                                              [:type :name :return-type 
                                               :parameters :body])]
                             (cond-> base-node
                               (:parameters base-node) 
                               (update :parameters 
                                       (fn [params] 
                                         (take 10 params)))
                               
                               (:body base-node) 
                               (update :body 
                                       (fn [body] 
                                         (take 100 body))))))
                         (take 100 nodes))))]
       {:type (:type processed-ast)
        :total-nodes (count (:nodes ast))
        :nodes (:nodes processed-ast)}))))
