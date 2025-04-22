(ns c51cc.lexer_test
  (:require [clojure.test :refer :all]
            [c51cc.logger :as log]
            [c51cc.lexer :refer :all]))

(deftest is-special-keyword
  (testing "Is Special Keyword"
    (log/debug "Начало теста: Проверка специальных ключевых слов")
    (is (= (is-special-keyword? "sfr") true))
    (is (= (is-special-keyword? "sbit") true))
    (is (= (is-special-keyword? "interrupt") true))
    (is (= (is-special-keyword? "using") true))
    (log/debug "Тест специальных ключевых слов завершен")))

(deftest is-type-keyword
  (testing "Is Type Keyword"
    (log/debug "Начало теста: Проверка ключевых слов типов")
    (is (= (is-type-keyword? "char") true))
    (is (= (is-type-keyword? "int") true))
    (is (= (is-type-keyword? "void") true))
    (is (= (is-type-keyword? "signed") true))
    (is (= (is-type-keyword? "unsigned") true))
    (log/debug "Тест ключевых слов типов завершен")))

(deftest is-separator-keyword
  (testing "Is Separator Keyword"
    (log/debug "Начало теста: Проверка разделителей")
    (is (= (is-separator-keyword? "(") true))
    (is (= (is-separator-keyword? ")") true))
    (is (= (is-separator-keyword? "{") true))
    (is (= (is-separator-keyword? "}") true))
    (is (= (is-separator-keyword? "[") true))
    (is (= (is-separator-keyword? "]") true))
    (is (= (is-separator-keyword? ",") true))
    (is (= (is-separator-keyword? ";") true))
    (is (= (is-separator-keyword? ":") true))
    (log/debug "Тест разделителей завершен")))

(deftest is-operator-keyword
  (testing "Is Operator Keyword"
    (log/debug "Начало теста: Проверка операторов")
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
    (is (= (is-operator-keyword? "%=") true))
    (log/debug "Тест операторов завершен")))

(deftest is-control-flow-keyword
  (testing "Is Control Flow Keyword"
    (log/debug "Начало теста: Проверка ключевых слов управления потоком")
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
    (is (= (is-control-flow-keyword? "goto") true))
    (log/debug "Тест ключевых слов управления потоком завершен")))

(deftest is-constant-keyword
  (testing "Is Constant Keyword"
    (log/debug "Начало теста: Проверка ключевых слов константы")
    (is (= (is-constant-keyword? "const") true))
    (log/debug "Тест ключевых слов константы завершен")))

(deftest is-identifier
  (testing "Is Identifier"
    (log/debug "Начало теста: Проверка идентификаторов")
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
    (is (= (is-identifier? "identifier_9_") true))
    (log/debug "Тест идентификаторов завершен")))

(deftest is-number
  (testing "Is Number"
    (log/debug "Начало теста: Проверка чисел")
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
    (is (= (is-number? "+0X") false))
    (log/debug "Тест чисел завершен")))

(deftest is-string
  (testing "Is String"
    (log/debug "Начало теста: Проверка строк")
    (is (= (is-string? "\"Hello, World!\"") true))
    (log/debug "Тест строк завершен")))

;; (deftest is-comment
;;   (testing "Is Comment"
;;     (is (= (is-comment? "// This is a comment") true))
;;     (is (= (is-comment? "/* This is a comment */") true))
;;     (is (= (is-comment? "/* This is a comment /*") false))
;;     (is (= (is-comment? "// This is a comment /*") true))))

(deftest is-preprocessor-directive
  (testing "Is Preprocessor Directive"
    (log/debug "Начало теста: Проверка препроцессорных директив")
    ;; Основные директивы препроцессора
    (is (= (is-preprocessor-directive? "#include") true))
    (is (= (is-preprocessor-directive? "#define") true))
    (is (= (is-preprocessor-directive? "#undef") true))
    
    ;; Условные директивы
    (is (= (is-preprocessor-directive? "#if") true))
    (is (= (is-preprocessor-directive? "#ifdef") true))
    (is (= (is-preprocessor-directive? "#ifndef") true))
    (is (= (is-preprocessor-directive? "#else") true))
    (is (= (is-preprocessor-directive? "#elif") true))
    (is (= (is-preprocessor-directive? "#endif") true))
    
    ;; Специальные директивы
    (is (= (is-preprocessor-directive? "#error") true))
    (is (= (is-preprocessor-directive? "#pragma") true))
    (is (= (is-preprocessor-directive? "#line") true))
    (is (= (is-preprocessor-directive? "#warning") true))
    
    ;; Негативные тесты
    (is (= (is-preprocessor-directive? "include") false))
    (is (= (is-preprocessor-directive? "#notadirective") false))
    (log/debug "Тест препроцессорных директив завершен")))

(deftest is-include-path-test
  (testing "Is Include Path"
    (log/debug "Начало теста: Проверка путей включения")
    ;; Стандартные заголовочные файлы
    (is (= (is-include-path? "<stdio.h>") true))
    (is (= (is-include-path? "<stdlib.h>") true))
    (is (= (is-include-path? "<complex/path/header.h>") true))
    
    ;; Пользовательские заголовочные файлы
    (is (= (is-include-path? "\"myheader.h\"") true))
    (is (= (is-include-path? "\"../include/header.h\"") true))
    (is (= (is-include-path? "\"./local/header.h\"") true))
    
    ;; Негативные тесты
    (is (= (is-include-path? "stdio.h") false))
    (is (= (is-include-path? "<incomplete") false))
    (is (= (is-include-path? "\"unclosed") false))
    (is (= (is-include-path? "<>") false))
    (is (= (is-include-path? "\"\"") false))
    (log/debug "Тест путей включения завершен")))

(deftest tokenize-test
  (testing "Basic tokenization"
    (log/debug "Начало теста: Базовая токенизация")
    
    ;; Тесты для препроцессорных директив
    (is (= [{:value "#include" :type :preprocessor-directive}
            {:value "<stdio.h>" :type :include-path}]
           (tokenize "#include <stdio.h>")))
           
    (is (= [{:value "#include" :type :preprocessor-directive}
            {:value "\"myheader.h\"" :type :include-path}]
           (tokenize "#include \"myheader.h\"")))
           
    (is (= [{:value "#define" :type :preprocessor-directive}
            {:value "MAX_SIZE" :type :identifier}
            {:value "100" :type :int_number}]
           (tokenize "#define MAX_SIZE 100")))
           
    (is (= [{:value "#ifdef" :type :preprocessor-directive}
            {:value "DEBUG" :type :identifier}]
           (tokenize "#ifdef DEBUG")))
    
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

    ;; Тесты для сложных выражений
    (is (= [{:value "int" :type :type-keyword}
            {:value "x" :type :identifier}
            {:value "=" :type :operator}
            {:value "5" :type :int_number}
            {:value "+" :type :operator}
            {:value "3" :type :int_number}
            {:value ";" :type :separator}]
           (tokenize "int x = 5 + 3;")))
    (log/debug "Тест токенизации завершен")))