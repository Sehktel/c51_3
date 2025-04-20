(ns c51cc.parser_test
  "Тесты для синтаксического анализатора"
  (:require [clojure.test :refer :all]
            [c51cc.logger :as log]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; Вспомогательная функция для создания токенов с логированием
(defn create-tokens
  "Создание списка токенов для тестирования с отладочным логированием

   Теоретическое обоснование:
   - Абстракция процесса создания токенов
   - Упрощение подготовки тестовых данных
   - Добавление механизма отладочного логирования"
  [& token-specs]
  (log/debug "Создание токенов:" token-specs)
  (let [tokens (mapv (fn [[type value]]
                       {:type type :value value})
                     token-specs)]
    (log/debug "Созданные токены:" tokens)
    tokens))

;; Тесты для парсинга программы
(deftest test-parse-program
  (log/set-debug-level! :DEBUG)
  (testing "Парсинг простой программы"
    (log/debug "Начало теста: Парсинг простой программы")
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "main"]
                  [:separator "("]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator "}"])
          result (parser/parse tokens)]
      (log/debug "Результат парсинга:" result)
      (is (= (:type result) :program))
      (is (= (count (:nodes result)) 1))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга функций
(deftest test-parse-function-declaration
  (testing "Корректное объявление функции"
    (log/set-debug-level! :TRACE)
    (log/debug "Начало теста: Корректное объявление функции")
    (let [tokens (create-tokens
                  [:type-keyword "void"]
                  [:identifier "testFunction"]
                  [:separator "("]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator ";"]
                  [:separator "}"])
          parser (parser/create-parser tokens)
          result (parser/parse-function-declaration parser tokens)]
      (log/debug "Результат парсинга функции:" result)
      (is (= (:type result) :function-declaration))
      (is (= (:return-type result) "void"))
      (is (= (:name result) "testFunction"))
      (log/debug "Тест завершен успешно")))

  (testing "Некорректное объявление функции"
    (log/debug "Начало теста: Некорректное объявление функции")
    (let [tokens (create-tokens
                  [:identifier "invalidFunction"]
                  [:separator "("])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Некорректное объявление функции"
           (let [parser (parser/create-parser tokens)]
             (parser/parse-function-declaration parser tokens))))
      (log/debug "Тест на некорректное объявление функции завершен"))))

;; Тесты для парсинга переменных
(deftest test-parse-variable-declaration
  (testing "Корректное объявление переменной"
    (log/debug "Начало теста: Корректное объявление переменной")
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "x"])
          parser (parser/create-parser tokens)
          result (parser/parse-variable-declaration parser tokens)]
      (log/debug "Результат парсинга переменной:" result)
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "int"))
      (is (= (:name result) "x"))
      (log/debug "Тест завершен успешно"))

  (testing "Некорректное объявление переменной"
    (log/debug "Начало теста: Некорректное объявление переменной")
    (let [tokens (create-tokens
                  [:identifier "y"])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Некорректное объявление переменной"
           (let [parser (parser/create-parser tokens)]
             (parser/parse-variable-declaration parser tokens))))
      (log/debug "Тест на некорректное объявление переменной завершен"))))

;; Тесты для парсинга выражений
(deftest test-parse-expression
  (testing "Парсинг идентификатора"
    (log/debug "Начало теста: Парсинг идентификатора")
    (let [tokens (create-tokens
                  [:identifier "variable"])
          parser (parser/create-parser tokens)
          result (parser/parse-expression parser tokens)]
      (log/debug "Результат парсинга идентификатора:" result)
      (is (= (:type result) :expression))
      (is (= (:value result) "variable"))
      (log/debug "Тест завершен успешно"))

  (testing "Парсинг числа"
    (log/debug "Начало теста: Парсинг числа")
    (let [tokens (create-tokens
                  [:int_number "42"])
          parser (parser/create-parser tokens)
          result (parser/parse-expression parser tokens)]
      (log/debug "Результат парсинга числа:" result)
      (is (= (:type result) :expression))
      (is (= (:value result) "42"))
      (log/debug "Тест завершен успешно"))

  (testing "Неподдерживаемое выражение"
    (log/debug "Начало теста: Неподдерживаемое выражение")
    (let [tokens (create-tokens
                  [:unknown "something"])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Неподдерживаемое выражение"
           (let [parser (parser/create-parser tokens)]
             (parser/parse-expression parser tokens))))
      (log/debug "Тест на неподдерживаемое выражение завершен")))))))

;; Запуск всех тестов
;; (defn run-tests
;;   "Функция для запуска всех тестов

;;    Архитектурные соображения:
;;    - Централизованный запуск тестов
;;    - Возможность расширения"
;;   []
;;   (clojure.test/run-tests 'c51cc.parser-test))