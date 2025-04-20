(ns c51cc.ast_test
  "Модуль тестирования абстрактного синтаксического дерева (AST)"
  (:require [clojure.test :refer :all]
            [c51cc.ast :as ast]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [c51cc.logger :as log]))

(deftest test-print-ast-node
  "Тестирование функции печати узла AST

  Цели тестирования:
  - Проверка корректности обработки различных типов узлов
  - Валидация рекурсивного обхода дерева
  - Отсутствие исключений при различных входных данных"
  (testing "Тестирование узла программы"
    (let [sample-program {:type :program
                          :nodes [{:type :function-declaration
                                   :name "main"
                                   :return-type "int"
                                   :parameters []
                                   :body []}]}]
      (is (do 
            (ast/print-ast-node sample-program)
            true) "Печать узла программы не должна вызывать исключений")))
  
  (testing "Тестирование узла объявления функции"
    (let [function-node {:type :function-declaration
                         :name "example_func"
                         :return-type "void"
                         :parameters [{:type "int" :name "param1"}
                                      {:type "char" :name "param2"}]
                         :body [{:value "return"} {:value "0"}]}]
      (is (do 
            (ast/print-ast-node function-node)
            true) "Печать узла функции не должна вызывать исключений")))
  
  (testing "Тестирование узла объявления переменной"
    (let [variable-node {:type :variable-declaration
                         :name "test_var"
                         :var-type "int"}]
      (is (do 
            (ast/print-ast-node variable-node)
            true) "Печать узла переменной не должна вызывать исключений")))
  
  (testing "Тестирование узла выражения"
    (let [expression-node {:type :expression
                           :value "42"}]
      (is (do 
            (ast/print-ast-node expression-node)
            true) "Печать узла выражения не должна вызывать исключений"))))

(deftest test-print-ast
  "Тестирование функции печати полного AST

  Теоретическое обоснование:
  - Проверка интеграции с лексером и парсером
  - Валидация обработки различных входных данных"
  (testing "Печать AST из строки кода"
    (let [sample-code "int main() { return 0; }"]
      (is (do 
            (ast/print-ast sample-code)
            true) "Печать AST из строки кода не должна вызывать исключений")))
  
  (testing "Печать AST из токенов"
    (let [tokens (lexer/tokenize "int main() { return 0; }")]
      (is (do 
            (ast/print-ast tokens)
            true) "Печать AST из токенов не должна вызывать исключений"))))

(deftest test-pretty-print-ast
  "Тестирование функции детальной печати AST

  Преимущества:
  - Проверка совместимости с pretty-print
  - Валидация обработки сложных структур"
  (testing "Pretty-print AST из строки кода"
    (let [sample-code "int main() { int x = 10; return x; }"]
      (is (do 
            (ast/pretty-print-ast sample-code)
            true) "Детальная печать AST не должна вызывать исключений")))
  
  (testing "Pretty-print AST из токенов"
    (let [tokens (lexer/tokenize "int main() { int x = 10; return x; }")]
      (is (do 
            (ast/pretty-print-ast tokens)
            true) "Детальная печать AST из токенов не должна вызывать исключений"))))

(deftest test-ast-edge-cases
  "Тестирование краевых случаев AST

  Методология:
  - Проверка обработки пограничных и неожиданных входных данных"
  (testing "Пустая программа"
    (let [empty-code ""]
      (is (try 
            (ast/print-ast empty-code)
            true
            (catch Exception e false)) "Обработка пустой программы")))
  
;;   (testing "Программа с комментариями"
;;     (let [commented-code "// Простой комментарий\nint main() { return 0; }"]
;;       (is (do 
;;             (ast/print-ast commented-code)
;;             true) "Обработка кода с комментариями")))
)

;; Настройка логирования для тестов
(defn setup-test-logging []
  (log/set-debug-level! :INFO))

;; Выполнение настройки перед запуском тестов
(setup-test-logging)

;; Запуск тестов
;; (run-tests 'c51cc.ast_test) 