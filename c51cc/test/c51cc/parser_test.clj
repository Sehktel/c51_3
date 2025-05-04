(ns c51cc.parser_test
  "Тесты для синтаксического анализатора C51.
   Использует функциональный подход к тестированию."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [c51cc.logger :as log]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [c51cc.test-utils :refer [test-parse-expression ;; парсинг выражений
                                    test-parse-binary-operation ;; парсинг бинарных операций
                                    test-parse-variable-declaration ;; парсинг объявлений переменных
                                    test-parse-function-declaration ;; парсинг объявлений функций
                                    test-parse-array-declaration ;; парсинг объявлений массивов
                                    test-parse-pointer-declaration ;; парсинг объявлений указателей
                                    test-parse-assignment ;; парсинг присваиваний
                                    test-parse-if-else ;; парсинг if-else
                                    test-parse-while-loop ;; парсинг while-loop
                                    test-parse-for-loop ;; парсинг for-loop
                                    test-parse-do-while ;; парсинг do-while
                                    test-parse-switch-case ;; парсинг switch-case
                                    test-parse-break-continue ;; парсинг break-continue
                                    test-parse-return ;; парсинг return
                                    ]]))

;; =============================================
;; Фикстуры и вспомогательные функции
;; =============================================

(defn setup-fixture
  "Фикстура для настройки тестового окружения"
  [f]
  (log/set-debug-level! :INFO)
  (f))

(use-fixtures :each setup-fixture)

(defn create-token
  "Создает токен с заданным типом и значением"
  [type value]
  {:type type :value value})

(defn create-tokens
  "Создает последовательность токенов для тестирования.
   Принимает пары [тип значение]."
  [& token-specs]
  (->> token-specs
       (partition 2)
       (mapv (fn [[type value]] (create-token type value)))))

(defn assert-node-type
  "Проверяет тип узла AST"
  [node expected-type]
  (is (= (:type node) expected-type)
      (str "Ожидался тип узла " expected-type ", получен " (:type node))))

(defn assert-node-value
  "Проверяет значение в узле AST по указанному пути"
  [node path expected-value]
  (is (= (get-in node path) expected-value)
      (str "Ожидалось значение " expected-value " по пути " path)))

;; =============================================
;; Тесты базовых выражений
;; =============================================

(deftest test-basic-expressions
  (testing "Парсинг идентификатора"
    (let [tokens (create-tokens :identifier "x")
          result (test-parse-expression tokens)]
      (assert-node-type result :identifier)
      (assert-node-value result [:value] "x")))

  (testing "Парсинг числового литерала"
    (let [tokens (create-tokens :int_number "42")
          result (test-parse-expression tokens)]
      (assert-node-type result :number)
      (assert-node-value result [:value] "42")))

  (testing "Парсинг строкового литерала"
    (let [tokens (create-tokens :string "\"test\"")
          result (test-parse-expression tokens)]
      (assert-node-type result :string)
      (assert-node-value result [:value] "\"test\""))))

;; =============================================
;; Тесты бинарных операций
;; =============================================

(deftest test-binary-operations
  (testing "Простая бинарная операция"
    (let [tokens (create-tokens 
                  :int_number "10" 
                  :operator "+" 
                  :int_number "5")
          result (test-parse-binary-operation tokens)]
      (assert-node-type result :binary-operation)
      (assert-node-value result [:operator] "+")
      (assert-node-value result [:left :value] "10")
      (assert-node-value result [:right :value] "5")))

  (testing "Бинарная операция с приоритетами"
    (let [tokens (create-tokens
                  :int_number "1"
                  :operator "+"
                  :int_number "2"
                  :operator "*"
                  :int_number "3")
          result (test-parse-binary-operation tokens)]
      (assert-node-type result :binary-operation)
      (assert-node-value result [:operator] "+")
      (let [right-node (get-in result [:right])]
        (assert-node-type right-node :binary-operation)
        (assert-node-value right-node [:operator] "*")))))

;; =============================================
;; Тесты объявлений переменных
;; =============================================

(deftest test-variable-declarations
  (testing "Простое объявление переменной"
    (let [tokens (create-tokens
                  :type-keyword "int"
                  :identifier "x"
                  :separator ";")
          result (test-parse-variable-declaration tokens)]
      (assert-node-type result :variable-declaration)
      (assert-node-value result [:var-type] "int")
      (assert-node-value result [:name] "x")))

  (testing "Объявление с инициализацией"
    (let [tokens (create-tokens
                  :type-keyword "int"
                  :identifier "x"
                  :operator "="
                  :int_number "42"
                  :separator ";")
          result (test-parse-variable-declaration tokens)]
      (assert-node-type result :variable-declaration)
      (assert-node-value result [:var-type] "int")
      (assert-node-value result [:name] "x")
      (assert-node-value result [:init-value :value] "42"))))

;; =============================================
;; Тесты объявлений функций
;; =============================================

(deftest test-function-declarations
  (testing "Объявление функции без параметров"
    (let [tokens (create-tokens
                  :type-keyword "void"
                  :identifier "test"
                  :separator "("
                  :separator ")"
                  :separator "{"
                  :separator "}")
          result (test-parse-function-declaration tokens)]
      (assert-node-type result :function-declaration)
      (assert-node-value result [:return-type :value] "void")
      (assert-node-value result [:name :value] "test")
      (is (empty? (:parameters result)))))

  (testing "Объявление функции с параметрами"
    (let [tokens (create-tokens
                  :type-keyword "int"
                  :identifier "sum"
                  :separator "("
                  :type-keyword "int"
                  :identifier "a"
                  :separator ","
                  :type-keyword "int"
                  :identifier "b"
                  :separator ")"
                  :separator "{"
                  :separator "}")
          result (parser/parse-function-declaration tokens)]
      (assert-node-type result :function-declaration)
      (assert-node-value result [:return-type :value] "int")
      (assert-node-value result [:name :value] "sum")
      (is (= (count (:parameters result)) 2))
      (is (every? #(= (:type %) "int") (:parameters result))))))

;; =============================================
;; Тесты управляющих конструкций
;; =============================================

(deftest test-control-structures
  (testing "Парсинг if-else"
    (let [tokens (create-tokens
                  :keyword "if"
                  :separator "("
                  :identifier "x"
                  :operator ">"
                  :int_number "0"
                  :separator ")"
                  :separator "{"
                  :separator "}"
                  :keyword "else"
                  :separator "{"
                  :separator "}")
          result (test-parse-if-else tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :if-else)
      (is (map? (:condition result)))
      (is (vector? (:true-block result)))
      (is (vector? (:false-block result)))))

  (testing "Парсинг while"
    (let [tokens (create-tokens
                  :keyword "while"
                  :separator "("
                  :identifier "x"
                  :operator ">"
                  :int_number "0"
                  :separator ")"
                  :separator "{"
                  :separator "}")
          result (test-parse-while-loop tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :while-loop)
      (is (map? (:condition result)))))

  (testing "Парсинг for"
    (let [tokens (create-tokens
                  :keyword "for"
                  :separator "("
                  :identifier "i"
                  :operator "="
                  :int_number "0"
                  :separator ";"
                  :identifier "i"
                  :operator "<"
                  :int_number "10"
                  :separator ";"
                  :identifier "i"
                  :operator "++"
                  :separator ")"
                  :separator "{"
                  :separator "}")
          result (test-parse-for-loop tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :for-loop)
      (is (map? (:initialization result)))
      (is (map? (:condition result)))
      (is (map? (:step result))))))

;; =============================================
;; Тесты массивов
;; =============================================

(deftest test-arrays
  (testing "Объявление массива"
    (let [tokens (create-tokens
                  :type-keyword "int"
                  :identifier "arr"
                  :separator "["
                  :int_number "10"
                  :separator "]"
                  :separator ";")
          result (test-parse-array-declaration tokens)]
      (assert-node-type result :variable-declaration)
      (assert-node-value result [:var-type] "int[]")
      (is (:is-array result))
      (assert-node-value result [:array-size :value] "10")))

  (testing "Инициализация массива"
    (let [tokens (create-tokens
                  :type-keyword "int"
                  :identifier "arr"
                  :separator "["
                  :int_number "3"
                  :separator "]"
                  :operator "="
                  :separator "{"
                  :int_number "1"
                  :separator ","
                  :int_number "2"
                  :separator ","
                  :int_number "3"
                  :separator "}"
                  :separator ";")
          result (test-parse-array-declaration tokens)]
      (assert-node-type result :variable-declaration)
      (assert-node-value result [:var-type] "int[]")
      (is (:is-array result))
      (let [init-values (get-in result [:init-values :values])]
        (is (= (count init-values) 3))
        (is (= (map :value init-values) ["1" "2" "3"]))))))

;; =============================================
;; Тесты указателей
;; =============================================

(deftest test-pointers
  (testing "Объявление указателя"
    (let [tokens (create-tokens
                  :type-keyword "int"
                  :operator "*"
                  :identifier "ptr"
                  :separator ";")
          result (test-parse-pointer-declaration tokens)]
      (assert-node-type result :variable-declaration)
      (assert-node-value result [:var-type] "int*")
      (is (:is-pointer result)))))

;; =============================================
;; Тесты присваиваний
;; =============================================

(deftest test-assignments
  (testing "Простое присваивание"
    (let [tokens (create-tokens
                  :identifier "x"
                  :operator "="
                  :int_number "42"
                  :separator ";")
          result (test-parse-assignment tokens)]
      (assert-node-type result :assignment)
      (assert-node-value result [:left :value] "x")
      (assert-node-value result [:operator] "=")
      (assert-node-value result [:right :value] "42")))

  (testing "Составное присваивание"
    (let [tokens (create-tokens
                  :identifier "x"
                  :operator "+="
                  :int_number "5"
                  :separator ";")
          result (test-parse-assignment tokens)]
      (assert-node-type result :assignment)
      (assert-node-value result [:left :value] "x")
      (assert-node-value result [:operator] "+=")
      (assert-node-value result [:right :value] "5")))

  (testing "Присваивание с выражением"
    (let [tokens (create-tokens
                  :identifier "result"
                  :operator "="
                  :identifier "a"
                  :operator "+"
                  :identifier "b"
                  :operator "*"
                  :int_number "2"
                  :separator ";")
          result (test-parse-assignment tokens)]
      (assert-node-type result :assignment)
      (assert-node-value result [:left :value] "result")
      (assert-node-value result [:operator] "=")
      (is (string? (get-in result [:right :value]))))))

;; =============================================
;; Тесты управляющих операторов
;; =============================================

(deftest test-control-operators
  (testing "Оператор break"
    (let [tokens (create-tokens
                  :keyword "break"
                  :separator ";")
          result (test-parse-break-continue tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :break)))

  (testing "Оператор continue"
    (let [tokens (create-tokens
                  :keyword "continue"
                  :separator ";")
          result (test-parse-break-continue tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :continue)))

  (testing "Оператор return"
    (let [tokens (create-tokens
                  :keyword "return"
                  :identifier "x"
                  :separator ";")
          result (test-parse-return tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :return)
      (is (map? (:value result))))))

;; =============================================
;; Тесты switch-case
;; =============================================

(deftest test-switch-case
  (testing "Конструкция switch-case"
    (let [tokens (create-tokens
                  :keyword "switch"
                  :separator "("
                  :identifier "x"
                  :separator ")"
                  :separator "{"
                  :keyword "case"
                  :int_number "1"
                  :separator ":"
                  :keyword "break"
                  :separator ";"
                  :keyword "default"
                  :separator ":"
                  :keyword "break"
                  :separator ";"
                  :separator "}")
          result (test-parse-switch-case tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :switch-case)
      (is (vector? (:cases result)))
      (is (map? (:default-case result))))))

;; =============================================
;; Тесты do-while
;; =============================================

(deftest test-do-while
  (testing "Цикл do-while"
    (let [tokens (create-tokens
                  :keyword "do"
                  :separator "{"
                  :separator "}"
                  :keyword "while"
                  :separator "("
                  :identifier "x"
                  :operator ">"
                  :int_number "0"
                  :separator ")"
                  :separator ";")
          result (test-parse-do-while tokens)]
      (assert-node-type result :control-flow)
      (assert-node-value result [:subtype] :do-while)
      (is (map? (:condition result))))))