(ns c51cc.ast-test
  "Модуль тестирования генерации и анализа абстрактного синтаксического дерева (AST)"
  (:require [clojure.test :refer :all]
            [c51cc.ast :as ast]
            [c51cc.logger :as log]
            [clojure.java.io :as io]))

;; Вспомогательные функции для тестирования
(defn- create-test-file 
  "Создает временный тестовый файл с заданным содержимым"
  [content]
  (let [temp-file (java.io.File/createTempFile "c51cc_test" ".c")]
    (spit temp-file content)
    (.deleteOnExit temp-file)
    temp-file))

(defn- cleanup-test-file 
  "Удаляет временный тестовый файл"
  [^java.io.File file]
  (when (and file (.exists file))
    (.delete file)))

;; Тестовые константы
(def ^:private simple-c-code 
  "Простой тестовый код на языке C
   для проверки базовой функциональности AST"
  "#include <stdio.h>

int main() {
    int x = 10;
    printf(\"Hello, World! %d\", x);
    return 0;
}")

(def ^:private complex-c-code
  "Более сложный тестовый код с различными конструкциями"
  "#include <stdlib.h>
#define MAX_SIZE 100

void process_array(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        if (arr[i] > MAX_SIZE) {
            arr[i] = MAX_SIZE;
        }
    }
}

int main() {
    int numbers[MAX_SIZE];
    int count = 0;
    
    while (count < MAX_SIZE) {
        numbers[count] = rand() % 200;
        count++;
    }
    
    process_array(numbers, MAX_SIZE);
    return 0;
}")

(deftest test-generate-ast
  "Тест генерации абстрактного синтаксического дерева"
  (testing "Генерация AST для простого кода"
    (let [test-file (create-test-file simple-c-code)
          result (ast/process-c-file (.getAbsolutePath test-file))]
      (is (map? result) "Результат должен быть картой")
      (is (contains? result :ast) "Результат должен содержать AST")
      (is (vector? (:ast result)) "AST должен быть вектором")
      (is (pos? (count (:ast result))) "AST не должен быть пустым")
      (cleanup-test-file test-file)))
  
  (testing "Генерация AST для сложного кода"
    (let [test-file (create-test-file complex-c-code)
          result (ast/process-c-file (.getAbsolutePath test-file))]
      (is (map? result) "Результат должен быть картой")
      (is (contains? result :ast) "Результат должен содержать AST")
      (is (vector? (:ast result)) "AST должен быть вектором")
      (is (pos? (count (:ast result))) "AST не должен быть пустым")
      (cleanup-test-file test-file))))

(deftest test-ast-analysis
  "Тест анализа абстрактного синтаксического дерева"
  (testing "Анализ AST простого кода"
    (let [test-file (create-test-file simple-c-code)
          result (ast/process-c-file (.getAbsolutePath test-file))
          analysis (ast/analyze-ast (:ast result))]
      (is (map? analysis) "Результат анализа должен быть картой")
      (is (contains? analysis :node-types) "Анализ должен содержать типы узлов")
      (is (contains? analysis :total-nodes) "Анализ должен содержать общее количество узлов")
      (is (contains? analysis :max-depth) "Анализ должен содержать максимальную глубину")
      (is (pos? (:total-nodes analysis)) "Должны быть узлы в AST")
      (cleanup-test-file test-file)))
  
  (testing "Анализ AST сложного кода"
    (let [test-file (create-test-file complex-c-code)
          result (ast/process-c-file (.getAbsolutePath test-file))
          analysis (ast/analyze-ast (:ast result))]
      (is (map? analysis) "Результат анализа должен быть картой")
      (is (contains? analysis :node-types) "Анализ должен содержать типы узлов")
      (is (contains? analysis :total-nodes) "Анализ должен содержать общее количество узлов")
      (is (contains? analysis :max-depth) "Анализ должен содержать максимальную глубину")
      (is (pos? (:total-nodes analysis)) "Должны быть узлы в AST")
      (cleanup-test-file test-file))))

(deftest test-pretty-print-ast
  "Тест визуализации абстрактного синтаксического дерева"
  (testing "Визуализация AST простого кода"
    (let [test-file (create-test-file simple-c-code)
          result (ast/process-c-file (.getAbsolutePath test-file))
          pretty-print (ast/pretty-print-ast (:ast result))]
      (is (string? pretty-print) "Визуализация должна быть строкой")
      (is (pos? (count pretty-print)) "Визуализация не должна быть пустой")
      (cleanup-test-file test-file)))
  
  (testing "Визуализация AST сложного кода"
    (let [test-file (create-test-file complex-c-code)
          result (ast/process-c-file (.getAbsolutePath test-file))
          pretty-print (ast/pretty-print-ast (:ast result))]
      (is (string? pretty-print) "Визуализация должна быть строкой")
      (is (pos? (count pretty-print)) "Визуализация не должна быть пустой")
      (cleanup-test-file test-file))))

(deftest test-error-handling
  "Тест обработки ошибок при генерации AST"
  (testing "Обработка несуществующего файла"
    (is (thrown? Exception 
                 (ast/process-c-file "/path/to/nonexistent/file.c"))
        "Должно быть выброшено исключение для несуществующего файла"))
  
  (testing "Обработка пустого файла"
    (let [empty-file (create-test-file "")]
      (is (thrown? Exception 
                   (ast/process-c-file (.getAbsolutePath empty-file)))
          "Должно быть выброшено исключение для пустого файла")
      (cleanup-test-file empty-file))))

(defn -main
  "Точка входа для запуска тестов AST"
  [& args]
  (log/set-level! :DEBUG)
  (run-tests 'c51cc.ast-test)) 