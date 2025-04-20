(ns c51cc.parser_test
  "Тесты для синтаксического анализатора"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; Вспомогательная функция для создания токенов
(defn create-tokens
  "Создание списка токенов для тестирования

   Теоретическое обоснование:
   - Абстракция процесса создания токенов
   - Упрощение подготовки тестовых данных"
  [& token-specs]
  (mapv (fn [[type value]]
          {:type type :value value})
        token-specs))

;; Тесты для парсинга программы
(deftest test-parse-program
  (testing "Парсинг простой программы"
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "main"]
                  [:separator "("]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator "}"])
          result (parser/parse tokens)]
      (is (= (:type result) :program))
      (is (= (count (:nodes result)) 1)))))

;; Тесты для парсинга функций
(deftest test-parse-function-declaration
  (testing "Корректное объявление функции"
    (let [tokens (create-tokens
                  [:type-keyword "void"]
                  [:identifier "testFunction"]
                  [:separator "("])
          parser (parser/create-parser tokens)
          result (parser/parse-function-declaration parser tokens)]
      (is (= (:type result) :function-declaration))
      (is (= (:return-type result) "void"))
      (is (= (:name result) "testFunction"))))

  (testing "Некорректное объявление функции"
    (let [tokens (create-tokens
                  [:identifier "invalidFunction"]
                  [:separator "("])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Некорректное объявление функции"
           (let [parser (parser/create-parser tokens)]
             (parser/parse-function-declaration parser tokens)))))))

;; Тесты для парсинга переменных
(deftest test-parse-variable-declaration
  (testing "Корректное объявление переменной"
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "x"])
          parser (parser/create-parser tokens)
          result (parser/parse-variable-declaration parser tokens)]
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "int"))
      (is (= (:name result) "x"))))

  (testing "Некорректное объявление переменной"
    (let [tokens (create-tokens
                  [:identifier "y"])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Некорректное объявление переменной"
           (let [parser (parser/create-parser tokens)]
             (parser/parse-variable-declaration parser tokens)))))))

;; Тесты для парсинга выражений
(deftest test-parse-expression
  (testing "Парсинг идентификатора"
    (let [tokens (create-tokens
                  [:identifier "variable"])
          parser (parser/create-parser tokens)
          result (parser/parse-expression parser tokens)]
      (is (= (:type result) :expression))
      (is (= (:value result) "variable"))))

  (testing "Парсинг числа"
    (let [tokens (create-tokens
                  [:int_number "42"])
          parser (parser/create-parser tokens)
          result (parser/parse-expression parser tokens)]
      (is (= (:type result) :expression))
      (is (= (:value result) "42"))))

  (testing "Неподдерживаемое выражение"
    (let [tokens (create-tokens
                  [:unknown "something"])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Неподдерживаемое выражение"
           (let [parser (parser/create-parser tokens)]
             (parser/parse-expression parser tokens)))))))

;; Запуск всех тестов
;; (defn run-tests
;;   "Функция для запуска всех тестов

;;    Архитектурные соображения:
;;    - Централизованный запуск тестов
;;    - Возможность расширения"
;;   []
;;   (clojure.test/run-tests 'c51cc.parser-test))