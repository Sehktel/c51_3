(ns c51cc.test-utils
  "Утилиты для тестирования приватных функций парсера.
   Предоставляет контролируемый доступ к внутренним функциям
   только для целей тестирования."
  (:require [c51cc.parser :as parser]))

;; Обертки для приватных функций парсера

(defn test-parse-parameters
  "Тестовая обертка для internal-parse-parameters.
   Используется только в тестах для проверки парсинга параметров функции."
  [input]
  (@#'c51cc.parser/internal-parse-parameters input))

(defn test-parse-variable-declaration
  "Тестовая обертка для parse-variable-declaration.
   Используется для проверки парсинга объявлений переменных."
  [input]
  (@#'c51cc.parser/parse-variable-declaration input))

(defn test-parse-expression
  "Тестовая обертка для parse-expression.
   Проверка парсинга выражений."
  [input]
  (@#'c51cc.parser/parse-expression input))

(defn test-parse-function-body
  "Тестовая обертка для internal-parse-function-body.
   Проверка парсинга тела функции."
  [input]
  (@#'c51cc.parser/internal-parse-function-body input))

(defn test-parse-function-arguments
  "Тестовая обертка для parse-function-arguments.
   Проверка парсинга аргументов функции."
  [input]
  (@#'c51cc.parser/parse-function-arguments input))

(defn test-parse-function-call
  "Тестовая обертка для parse-function-call.
   Проверка парсинга вызова функции."
  [input]
  (@#'c51cc.parser/parse-function-call input))

(defn test-parse-binary-operation
  "Тестовая обертка для parse-binary-operation.
   Проверка парсинга бинарных операций."
  [input]
  (@#'c51cc.parser/parse-binary-operation input))

(defn test-parse-unary-operation
  "Тестовая обертка для parse-unary-operation.
   Проверка парсинга унарных операций."
  [input]
  (@#'c51cc.parser/parse-unary-operation input))

(defn test-parse-bitwise-operation
  "Тестовая обертка для parse-bitwise-operation.
   Проверка парсинга побитовых операций."
  [input]
  (@#'c51cc.parser/parse-bitwise-operation input))

(defn test-parse-array-initializer
  "Тестовая обертка для parse-array-initializer.
   Проверка парсинга инициализации массивов."
  [input]
  (@#'c51cc.parser/parse-array-initializer input))

;; Предикаты для проверки операторов

(defn test-is-binary-operator?
  "Тестовая обертка для is-binary-operator?.
   Проверка определения бинарных операторов."
  [operator]
  (@#'c51cc.parser/is-binary-operator? operator))

(defn test-is-unary-operator?
  "Тестовая обертка для is-unary-operator?.
   Проверка определения унарных операторов."
  [operator]
  (@#'c51cc.parser/is-unary-operator? operator))

(defn test-is-bitwise-operator?
  "Тестовая обертка для is-bitwise-operator?.
   Проверка определения побитовых операторов."
  [operator]
  (@#'c51cc.parser/is-bitwise-operator? operator)) 