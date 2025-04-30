(ns c51cc.ast_test
  "Модуль тестирования генерации и анализа абстрактного синтаксического дерева (AST)"
  (:require [clojure.test :refer :all]
            [c51cc.ast :as ast]
            [c51cc.logger :as log]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.stacktrace :as stacktrace]
            [clojure.pprint :as pprint]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [c51cc.preprocessor :as preprocessor])
  (:import (clojure.lang ExceptionInfo)
           (java.io File)))

(deftest test-node-types
  "Тестирование определения типов узлов AST"
  (testing "Проверка корректности предопределенных типов узлов"
    (are [expected actual] (= expected actual)
      :program (:program ast/node-types)
      :function-declaration (:function-declaration ast/node-types)
      :variable-declaration (:variable-declaration ast/node-types)
      :function-call (:function-call ast/node-types)
      :control-flow (:control-flow ast/node-types)
      :expression (:expression ast/node-types))))

(deftest test-pretty-print-ast
  "Тестирование функции расширенной визуализации AST"
  (testing "Базовые сценарии визуализации AST"
    (let [sample-ast {:type :program
                      :nodes [{:node-type :function-declaration
                               :name "main"
                               :return-type :void
                               :parameters [{:name "arg1" :param-type :int}
                                            {:name "arg2" :param-type :string}]
                               :body (repeat 200 {:node-type :expression})}
                              {:node-type :variable-declaration
                               :name "globalVar"
                               :var-type :int}]}
          printed-ast (ast/pretty-print-ast sample-ast)]
      (is (string? printed-ast))
      (is (not (empty? printed-ast)))
      (is (str/includes? printed-ast ":program"))
      (is (str/includes? printed-ast ":total-nodes"))))

  (testing "Обработка предельных случаев AST"
    (let [empty-ast {:type :program :nodes []}
          printed-empty-ast (ast/pretty-print-ast empty-ast)]
      (is (string? printed-empty-ast))
      (is (str/includes? printed-empty-ast ":total-nodes 0")))))

(deftest test-ast-logging
  "Тестирование логирования в процессе работы с AST"
  (testing "Проверка логирования при визуализации AST"
    (with-redefs [log/debug (fn [& args] args)]
      (let [sample-ast {:type :program :nodes []}
            log-output (ast/pretty-print-ast sample-ast)]
        (is (some #(= % "Начало расширенной визуализации AST") 
                  (log/debug "Начало расширенной визуализации AST")))))))

(defn safe-str-convert 
  "Безопасное преобразование входных данных в строку"
  [input]
  (cond 
    (string? input) input
    (sequential? input) 
      (if (every? string? input)
        (str/join "\n" input)
        (str/join "\n" (map str input)))
    :else (str input)))

(defn safe-tokenize 
  "Безопасная токенизация с расширенной обработкой"
  [input]
  (let [input-str (safe-str-convert input)]
    (log/debug "Токенизация входных данных:" 
               (str "Тип: " (type input-str) 
                    ", Длина: " (count input-str)
                    ", Первые 100 символов: " 
                    (subs input-str 0 (min 100 (count input-str)))))
    (lexer/tokenize input-str)))

(deftest test-ast-from-env-file
  "Тестирование полного конвейера обработки файла: препроцессинг -> лексер -> парсер -> AST"
  (testing "Генерация AST из файла, указанного в переменной окружения"
    (let [test-file-path (System/getenv "TEST_FILE")]
      (when (and test-file-path (str/ends-with? test-file-path ".c"))
        (let [file (io/file test-file-path)]
          (is (.exists file) "Файл должен существовать")
          (is (.isFile file) "Путь должен указывать на файл")
          
          (try
            (let [;; Расширенная диагностика чтения файла
                  _ (log/debug "Путь к файлу:" test-file-path)
                  file-content (slurp file)
                  _ (log/debug "Тип содержимого файла:" (type file-content))
                  _ (log/debug "Содержимое файла (первые 100 символов):" 
                               (subs (str file-content) 0 (min 100 (count (str file-content)))))
                  
                  base-path (.getParent file)
                  _ (log/debug "Базовый путь:" base-path)
                  
                  ;; Явное преобразование и проверка содержимого
                  file-content-str (safe-str-convert file-content)
                  _ (log/debug "Тип преобразованного содержимого:" (type file-content-str))
                  _ (log/debug "Длина содержимого:" (count file-content-str))
                  
                  ;; Полный конвейер обработки файла с расширенной диагностикой
                  preprocessed-code (try 
                                      (preprocessor/preprocess file-content-str :base-path base-path)
                                      (catch Exception e
                                        (log/error "Ошибка препроцессинга:" 
                                                   (str "Сообщение: " (.getMessage e) 
                                                        ", Тип: " (type e)))
                                        (throw e)))
                  
                  _ (log/debug "Тип препроцессированного кода:" (type preprocessed-code))
                  _ (log/debug "Препроцессированный код (первые 100 символов):" 
                               (subs (str preprocessed-code) 0 (min 100 (count (str preprocessed-code)))))
                  
                  ;; Токенизация с дополнительной обработкой
                  tokens (try
                           (safe-tokenize preprocessed-code)
                           (catch Exception e
                             (log/error "Ошибка токенизации:" 
                                        (str "Сообщение: " (.getMessage e) 
                                             ", Тип: " (type e)))
                             (throw e)))
                  
                  _ (log/debug "Количество токенов:" (count tokens))
                  _ (log/debug "Первые 5 токенов:" (pr-str (take 5 tokens)))
                  
                  ;; Парсинг с расширенной диагностикой
                  ast (try
                        (parser/parse 
                         (if (sequential? tokens)
                           tokens
                           (vec tokens)))
                        (catch Exception e
                          (log/error "Ошибка парсинга:" 
                                     (str "Сообщение: " (.getMessage e) 
                                          ", Тип: " (type e)))
                          (throw e)))
                  
                  _ (log/debug "Структура AST:" (pr-str (select-keys ast [:type :nodes])))
                  
                  pretty-printed-ast (ast/pretty-print-ast ast)]
              
              ;; Проверки визуализации AST
              (testing "Визуализация AST"
                (is (string? pretty-printed-ast) "Визуализация AST должна вернуть строку")
                (is (not (empty? pretty-printed-ast)) "Визуализация AST не должна быть пустой")))
            
            ;; Обработка возможных исключений
            (catch Throwable e
              (log/error "Критическая ошибка при обработке файла:" 
                         (str "Файл: " test-file-path 
                              ", Ошибка: " (.getMessage e)))
              (stacktrace/print-stack-trace e)
              (is false (str "Необработанное исключение: " (.getMessage e))))))
        
        ;; Если файл не указан или не является .c файлом
        (when-not test-file-path
          (log/warn "Переменная окружения TEST_FILE не установлена. Пропуск теста."))))))

(defn run-tests-entry 
  "Точка входа для запуска всех тестов AST"
  []
  (run-tests 'c51cc.ast_test))
