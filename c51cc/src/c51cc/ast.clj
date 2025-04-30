(ns c51cc.ast
  "Модуль для генерации и анализа абстрактного синтаксического дерева (AST)
   
   Расширенные возможности:
   - Полный цикл обработки файлов
   - Расширенная диагностика
   - Гибкий препроцессинг и токенизация"
  (:require 
            [c51cc.logger :as log]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :as stacktrace]
            [clojure.string :as str]
            [c51cc.lexer :as lexer]
            [c51cc.preprocessor :as preprocessor]
            [clojure.java.io :as io]))

;; Добавляем динамическую переменную для управления выводом токенов
(def ^:dynamic *debug-tokens* false)

(def node-types
  "Типы узлов абстрактного синтаксического дерева (AST)"
  {:program :program
   :function-declaration :function-declaration
   :variable-declaration :variable-declaration
   :function-call :function-call
   :control-flow :control-flow
   :expression :expression})

(declare pretty-print-ast)

(defn process-file-to-ast
  "Обрабатывает файл и возвращает AST"
  [file-path]
  (when (and file-path (str/ends-with? file-path ".c"))
    (let [file (io/file file-path)]
      (if (and (.exists file) (.isFile file))
        (try
          (let [file-content (slurp file)
                base-path (.getParent file)
                preprocessed-code (preprocessor/preprocess file-content :base-path base-path)
                tokens (lexer/tokenize preprocessed-code)
                parser-ns (requiring-resolve 'c51cc.parser/parse)
                ast (parser-ns tokens)]
            ast)
          (catch Exception e
            (println "\nОшибка при обработке файла:" file-path)
            (println "Сообщение:" (.getMessage e))
            (stacktrace/print-stack-trace e)
            nil))
        (do
          (println "\nФайл не существует или не является .c файлом:" file-path)
          nil)))))

(defn view-ast-from-file
  "Отображает AST дерево для указанного файла.
   Использует переменную окружения TEST_FILE если путь не указан."
  ([]
   (view-ast-from-file (System/getenv "TEST_FILE")))
  ([file-path]
   (when-let [ast (process-file-to-ast file-path)]
     (println "\nПолученное AST дерево:")
     (println "==================")
     (println (pretty-print-ast ast))
     (println "==================\n")
     ast)))

(defn- remove-tokens
  "Рекурсивно удаляет токены из структуры AST
   
   Теоретическая сложность: O(n), где n - общее количество узлов"
  [node]
  (cond
    (map? node) (let [node-without-tokens (dissoc node :tokens)]
                  (reduce-kv (fn [m k v]
                             (assoc m k (remove-tokens v)))
                           {}
                           node-without-tokens))
    (sequential? node) (mapv remove-tokens node)
    :else node))

(defn pretty-print-ast
  "Расширенная визуализация AST с глубоким форматированием и анализом

   Теоретические аспекты:
   - Многоуровневая визуализация структуры
   - Ограничение глубины для читаемости
   - Семантическая аннотация узлов
   - Условный вывод токенов на основе *debug-tokens*

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
                         (take 100 nodes))))
           final-ast (if *debug-tokens*
                      processed-ast
                      (remove-tokens processed-ast))]
       {:type (:type final-ast)
        :total-nodes (count (:nodes ast))
        :nodes (:nodes final-ast)}))))

(defn set-debug-tokens!
  "Устанавливает режим отображения токенов в AST
   
   Параметры:
   debug? - булево значение, true для отображения токенов"
  [debug?]
  (alter-var-root #'*debug-tokens* (constantly debug?)))
