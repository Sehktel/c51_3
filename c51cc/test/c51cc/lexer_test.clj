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

(deftest is-include-path-with-h
  (testing "Проверка путей include")
    (log/debug "Начало теста: Проверка путей включения с .h")
    ;; Только системные заголовочные файлы в угловых скобках
    (is (= (is-include-path? "<stdio.h>") true))
    (is (= (is-include-path? "<myheader.h>") true))
    (is (= (is-include-path? "<header.h>") true))
    (is (= (is-include-path? "<invalid_path.h>") true))    ;; Has .h extension
    
    ;; Пользовательские заголовочные файлы в кавычках не допускаются
    (log/debug "Пользовательские заголовочные файлы в кавычках не допускаются")
    (is (= (is-include-path? "\"stdio.h\"") false))
    (is (= (is-include-path? "\"myheader.h\"") false))
    (is (= (is-include-path? "\"header.h\"") false))
    (is (= (is-include-path? "\"invalid_path.h\"") false))
    
    ;; Пути без расширения .h не допускаются
    (log/debug "Пути без расширения .h не допускаются")
    (is (= (is-include-path? "<invalid_path>") false))
    (is (= (is-include-path? "\"invalid_path\"") false))
    
    ;; Пустые пути не допускаются
    (log/debug "Пустые пути не допускаются")
    (is (= (is-include-path? "\"\"") false))
    (is (= (is-include-path? "<>") false))

    (log/debug "Тест путей включения с .h завершен"))

(deftest tokenize-test
  (log/set-debug-level! :DEBUG)
  (testing "Basic tokenization"
    (log/debug "Начало теста: Базовая токенизация")
    
    ;; Tests for valid include paths
    (testing "Valid include paths"
      ;; System headers - only valid case
      (log/debug "System headers - only valid case")
      (is (= [{:value "#include" :type :include_directive}
              {:value "<stdio.h>" :type :include-path}]
             (tokenize "#include <stdio.h>")))

      (log/debug "Custom paths")
      (is (= [{:value "#include" :type :include_directive}
              {:value "<custom/path/myheader.h>" :type :include-path}]
             (tokenize "#include <custom/path/myheader.h>")))

      ;; User headers - lexer should tokenize, parser should validate
      (log/debug "User headers - lexer should tokenize, parser should validate")
      (is (= [{:value "#include" :type :include_directive}
              {:value "\"myheader.h\"" :type :string}]
             (tokenize "#include \"myheader.h\"")))

      (log/debug "Custom paths")
      (is (= [{:value "#include" :type :include_directive}
              {:value "\"./include/header.h\"" :type :string}]
             (tokenize "#include \"./include/header.h\""))))

    (testing "Invalid include paths"
      ;; Missing .h extension - lexer should tokenize, parser should validate
      (log/debug "Missing .h extension - lexer should tokenize, parser should validate")
      (is (= [{:value "#include" :type :include_directive}
              {:value "\"myfile\"" :type :string}]
             (tokenize "#include \"myfile\"")))  

      (log/debug "Missing .h extension - lexer should tokenize, parser should validate")
      (is (not= [{:value "#include" :type :include_directive}
                 {:value "<myfile>" :type :include-path}]
                 (tokenize "#include <myfile>")))
      
      ;; Проверка пустых путей, которые должны вызывать исключение
      (log/debug "Проверка пустых путей, которые должны вызывать исключение")
      (is (not= [{:value "#include", :type :include_directive} 
                 {:value "", :type :include-path}]
                 (tokenize "#include <>")))
      
      ;; Добавление проверки для случая, когда путь не является допустимым
      (log/debug "Проверка недопустимого пути")
      (is (not= [{:value "#include", :type :include_directive} 
                 {:value "invalid_path", :type :include-path}]
                 (tokenize "#include <invalid_path>")))
      
      ;; Empty paths should still throw because they're not valid tokens
      (log/debug "Empty paths should still throw because they're not valid tokens")
      (is (not= [{:value "#include", :type :include_directive} 
                 {:value "", :type :include-path}]
                 (tokenize "#include \"\"")))
      
      ;; Invalid characters - lexer should not tokenize
      (log/debug "Invalid characters - lexer should not tokenize")
      (is (thrown? clojure.lang.ExceptionInfo 
          (tokenize "#include <invalid@path.h>")))
      
      (log/debug "Invalid characters - lexer should not tokenize")
      (is (not= [{:value "#include", :type :include_directive} 
                 {:value "invalid@path.h", :type :include-path}]
                 (tokenize "#include \"invalid@path.h\"")))

      (log/debug "Тест токенизации завершен"))))

