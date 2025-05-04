(ns c51cc.test-utils
  "Утилиты для тестирования приватных функций парсера.
   Предоставляет контролируемый доступ к внутренним функциям
   только для целей тестирования."
  (:require [c51cc.parser :as parser]))

;; Обертки для функций парсера

(defn test-parse-expression
  "Тестовая обертка для parse-expression.
   Проверка парсинга выражений."
  [input]
  (parser/parse-expression input))

(defn test-parse-binary-operation
  "Тестовая обертка для parse-binary-operation.
   Проверка парсинга бинарных операций."
  [input]
  (parser/parse-binary-operation input 0))

(defn test-parse-variable-declaration
  "Тестовая обертка для parse-variable-declaration.
   Используется для проверки парсинга объявлений переменных."
  [input]
  (parser/parse-variable-declaration input))

(defn test-parse-function-declaration
  "Тестовая обертка для parse-function-declaration.
   Проверка парсинга объявлений функций."
  [input]
  (parser/parse-function-declaration input))

(defn test-parse-array-declaration
  "Тестовая обертка для parse-array-declaration.
   Проверка парсинга объявлений массивов."
  [input]
  (parser/parse-array-declaration input))

(defn test-parse-pointer-declaration
  "Тестовая обертка для parse-pointer-declaration.
   Проверка парсинга объявлений указателей."
  [input]
  (parser/parse-pointer-declaration input))

(defn test-parse-assignment
  "Тестовая обертка для parse-assignment.
   Проверка парсинга операций присваивания."
  [input]
  (parser/parse-assignment input))

(defn test-parse-if-else
  "Тестовая обертка для parse-if-else.
   Проверка парсинга условных конструкций."
  [input]
  (parser/parse-if-else input))

(defn test-parse-while-loop
  "Тестовая обертка для parse-while-loop.
   Проверка парсинга циклов while."
  [input]
  (parser/parse-while-loop input))

(defn test-parse-for-loop
  "Тестовая обертка для parse-for-loop.
   Проверка парсинга циклов for."
  [input]
  (parser/parse-for-loop input))

(defn test-parse-do-while
  "Тестовая обертка для parse-do-while.
   Проверка парсинга циклов do-while."
  [input]
  (parser/parse-do-while input))

(defn test-parse-switch-case
  "Тестовая обертка для parse-switch-case.
   Проверка парсинга конструкций switch-case."
  [input]
  (parser/parse-switch-case input))

(defn test-parse-break-continue
  "Тестовая обертка для parse-break-continue.
   Проверка парсинга операторов break и continue."
  [input]
  (parser/parse-break-continue input))

(defn test-parse-return
  "Тестовая обертка для parse-return.
   Проверка парсинга оператора return."
  [input]
  (parser/parse-return input))

(defn test-parse-function-body
  "Тестовая обертка для parse-function-body.
   Проверка парсинга тела функции."
  [input]
  (parser/parse-function-body input))

(defn test-parse-function-arguments
  "Тестовая обертка для parse-arguments.
   Проверка парсинга аргументов функции."
  [input]
  (parser/parse-arguments input))

(defn test-parse-function-call
  "Тестовая обертка для parse-function-call.
   Проверка парсинга вызова функции."
  [input]
  (parser/parse-function-call input))

(defn test-parse-unary-operation
  "Тестовая обертка для parse-unary-operation.
   Проверка парсинга унарных операций."
  [input]
  (parser/parse-unary-expression input))

(defn test-parse-bitwise-operation
  "Тестовая обертка для parse-binary-operation.
   Проверка парсинга побитовых операций."
  [input]
  (parser/parse-binary-operation input 0))

(defn test-parse-array-initializer
  "Тестовая обертка для parse-array-declaration.
   Проверка парсинга инициализации массивов."
  [input]
  (parser/parse-array-declaration input))

;; Предикаты для проверки операторов

(defn test-is-binary-operator?
  "Тестовая обертка для is-binary-operator?.
   Проверка определения бинарных операторов."
  [operator]
  (contains? (set (keys parser/operator-precedence)) operator))

(defn test-is-unary-operator?
  "Тестовая обертка для is-unary-operator?.
   Проверка определения унарных операторов."
  [operator]
  (contains? #{"!" "~" "++" "--"} operator))

(defn test-is-bitwise-operator?
  "Тестовая обертка для is-bitwise-operator?.
   Проверка определения побитовых операторов."
  [operator]
  (contains? #{"<<" ">>" "&" "|" "^"} operator)) 