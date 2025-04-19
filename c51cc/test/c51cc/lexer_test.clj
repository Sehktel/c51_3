(ns c51cc.lexer_test
  (:require [clojure.test :refer :all]
            [c51cc.lexer :refer :all]))

(deftest is-special-keyword
  (testing "Is Special Keyword"
    (is (= (is-special-keyword? "sfr") true))
    (is (= (is-special-keyword? "sbit") true))
    (is (= (is-special-keyword? "interrupt") true))
    (is (= (is-special-keyword? "using") true))))

(deftest is-type-keyword
  (testing "Is Type Keyword"
    (is (= (is-type-keyword? "char") true))
    (is (= (is-type-keyword? "int") true))
    (is (= (is-type-keyword? "void") true))
    (is (= (is-type-keyword? "signed") true))
    (is (= (is-type-keyword? "unsigned") true))))

(deftest is-separator-keyword
  (testing "Is Separator Keyword"
    (is (= (is-separator-keyword? "(") true))
    (is (= (is-separator-keyword? ")") true))
    (is (= (is-separator-keyword? "{") true))
    (is (= (is-separator-keyword? "}") true))
    (is (= (is-separator-keyword? "[") true))
    (is (= (is-separator-keyword? "]") true))
    (is (= (is-separator-keyword? ",") true))
    (is (= (is-separator-keyword? ";") true))
    (is (= (is-separator-keyword? ":") true))))

(deftest is-operator-keyword
  (testing "Is Operator Keyword"
    (is (= (is-operator-keyword? "+") true))
    (is (= (is-operator-keyword? "-") true))
    (is (= (is-operator-keyword? "*") true))
    (is (= (is-operator-keyword? "/") true))
    (is (= (is-operator-keyword? "%") true))
    (is (= (is-operator-keyword? "==") true))
    (is (= (is-operator-keyword? "!=") true))
    (is (= (is-operator-keyword? "<") true))
    (is (= (is-operator-keyword? ">") true))
    (is (= (is-operator-keyword? "<=") true))
    (is (= (is-operator-keyword? ">=") true))
    (is (= (is-operator-keyword? "&&") true))
    (is (= (is-operator-keyword? "||") true))
    (is (= (is-operator-keyword? "!") true))
    (is (= (is-operator-keyword? "&") true))
    (is (= (is-operator-keyword? "|") true))
    (is (= (is-operator-keyword? "^") true))
    (is (= (is-operator-keyword? "~") true))
    (is (= (is-operator-keyword? "++") true))
    (is (= (is-operator-keyword? "--") true))
    (is (= (is-operator-keyword? "=") true))
    (is (= (is-operator-keyword? "+=") true))
    (is (= (is-operator-keyword? "-=") true))
    (is (= (is-operator-keyword? "*=") true))
    (is (= (is-operator-keyword? "/=") true))
    (is (= (is-operator-keyword? "%=") true))))

(deftest is-control-flow-keyword
  (testing "Is Control Flow Keyword"
    (is (= (is-control-flow-keyword? "if") true))
    (is (= (is-control-flow-keyword? "else") true))
    (is (= (is-control-flow-keyword? "switch") true))
    (is (= (is-control-flow-keyword? "case") true))
    (is (= (is-control-flow-keyword? "default") true))
    (is (= (is-control-flow-keyword? "for") true))
    (is (= (is-control-flow-keyword? "while") true))
    (is (= (is-control-flow-keyword? "do") true))
    (is (= (is-control-flow-keyword? "break") true))
    (is (= (is-control-flow-keyword? "continue") true))
    (is (= (is-control-flow-keyword? "return") true))
    (is (= (is-control-flow-keyword? "goto") true))))

(deftest is-constant-keyword
  (testing "Is Constant Keyword"
    (is (= (is-constant-keyword? "const") true))))

(deftest is-identifier
  (testing "Is Identifier"
    (is (= (is-identifier? "identifier") true))
    (is (= (is-identifier? "identifier2") true))
    (is (= (is-identifier? "identifier_3") true))
    (is (= (is-identifier? "_identifier") true))
    (is (= (is-identifier? "_4_identifier") true))
    (is (= (is-identifier? "5identifier") false))
    (is (= (is-identifier? "identifier 6") false))
    (is (= (is-identifier? "identifier-7") false))
    (is (= (is-identifier? "identifier_") true))
    (is (= (is-identifier? "8") false))
    (is (= (is-identifier? "_") true))
    (is (= (is-identifier? "-") false))
    (is (= (is-identifier? " ") false))
    (is (= (is-identifier? "") false))
    (is (= (is-identifier? "identifier_9_") true))))

(deftest is-number
  (testing "Is Number"
    (is (= (is-number? "1") true))
    (is (= (is-number? "123") true))
    (is (= (is-number? "-456") true))
    (is (= (is-number? "+789") true))
    (is (= (is-number? "0x1A") true))
    (is (= (is-number? "-0x1B") false))
    (is (= (is-number? "+0x1C") false))
    (is (= (is-number? "0X1D") true))
    (is (= (is-number? "-0X1E") false))
    (is (= (is-number? "+0X1F") false))
    (is (= (is-number? "0x2a") true))
    (is (= (is-number? "-0x2b") false))
    (is (= (is-number? "+0x2c") false))
    (is (= (is-number? "0X2d") true))
    (is (= (is-number? "-0X2e") false))
    (is (= (is-number? "+0x") false))
    (is (= (is-number? "0X") false))
    (is (= (is-number? "-0X") false))
    (is (= (is-number? "+0X") false))))

(deftest is-string
  (testing "Is String"
    (is (= (is-string? "\"Hello, World!\"") true))))

;; (deftest is-comment
;;   (testing "Is Comment"
;;     (is (= (is-comment? "// This is a comment") true))
;;     (is (= (is-comment? "/* This is a comment */") true))
;;     (is (= (is-comment? "/* This is a comment /*") false))
;;     (is (= (is-comment? "// This is a comment /*") true))))

(deftest tokenize-test
  (testing "Basic tokenization"
    (is (= [{:value "int" :type :type-keyword}]
           (tokenize "int")))
    (is (= [{:value "a" :type :identifier}]
           (tokenize "a")))
    (is (= [{:value "0xfa" :type :hex_number}]
           (tokenize "0xfa")))
    (is (= [{:value "5" :type :int_number}]
           (tokenize "5")))
    
    ;; Тесты для составных токенов
    (is (= [{:value "int" :type :type-keyword}
            {:value "a" :type :identifier}]
           (tokenize "int a")))
    
    (is (= [{:value "int" :type :type-keyword}
            {:value "a" :type :identifier}
            {:value "=" :type :operator}
            {:value "5" :type :int_number}]
           (tokenize "int a = 5")))
    
    ;; Тесты для специальных ключевых слов
    (is (= [{:value "sfr" :type :special-keyword}]
           (tokenize "sfr")))
    
    (is (= [{:value "interrupt" :type :special-keyword}]
           (tokenize "interrupt")))
    
    ;; Тесты для операторов
    (is (= [{:value "+=" :type :operator}]
           (tokenize "+=")))
    
    ;; Тесты для разделителей
    (is (= [{:value "(" :type :separator}
            {:value ")" :type :separator}]
           (tokenize "( )")))
    
    ;; Тесты для управляющих конструкций
    (is (= [{:value "if" :type :control-flow}]
           (tokenize "if")))
    
    ;; Тесты для идентификаторов
    (is (= [{:value "variable_name" :type :identifier}]
           (tokenize "variable_name")))
    
    ;; Тесты для шестнадцатеричных чисел
    (is (= [{:value "0x1A" :type :hex_number}]
           (tokenize "0x1A")))
    
    ;; Тесты для строковых литералов
    (is (= [{:value "\"Hello\"" :type :string}]
           (tokenize "\"Hello\"")))
    
    ;; ;; Тесты для комментариев
    ;; (is (= [{:value "// Comment" :type :comment}]
    ;;        (tokenize "// Comment")))
    
    ;; Тесты для сложных выражений
    (is (= [{:value "int" :type :type-keyword}
            {:value "x" :type :identifier}
            {:value "=" :type :operator}
            {:value "5" :type :int_number}
            {:value "+" :type :operator}
            {:value "3" :type :int_number}
            {:value ";" :type :separator}]
           (tokenize "int x = 5 + 3;")))
    ))

