(ns c51cc.parser_test
  "Тесты для PEG-синтаксического анализатора"
  (:require [clojure.test :refer :all]
            [c51cc.logger :as log]
            [c51cc.parser :as parser]
            [instaparse.core :as insta]
            [clojure.string :as string]))

;; Вспомогательная функция для создания входных данных
(defn create-input 
  "Создание входных данных для парсинга с использованием строк"
  [& input-parts]
  (string/join " " input-parts))

;; Тесты для базовых конструкций языка
(deftest test-basic-parsing
  (testing "Парсинг простой функции без параметров"
    (let [input (create-input 
                 "int main() {"
                 "}")]
      (let [result (parser/parse input)]
        (is (not (:error result)) "Парсинг должен пройти без ошибок")
        (is (get-in result [:ast]) "AST должен быть сгенерирован"))))

  (testing "Парсинг функции с параметрами"
    (let [input (create-input 
                 "void testFunction(int x, char y) {"
                 "  int z = x + y;"
                 "}")]
      (let [result (parser/parse input)]
        (is (not (:error result)) "Парсинг должен пройти без ошибок")
        (is (get-in result [:ast]) "AST должен быть сгенерирован")))))

;; Тесты для различных типов деклараций
(deftest test-declarations
  (testing "Объявление переменных различных типов"
    (let [input (create-input 
                 "int x = 10;"
                 "int y = 10;"
                 "char z;"
                 "unsigned int a = 10;")]
      (let [result (parser/parse input)
            _ (println "Полный результат парсинга:" result)
            transformed-result (parser/transform-ast (:ast result))
            _ (println "Трансформированный результат:" transformed-result)]
        (is (not (:error result)) "Парсинг должен пройти без ошибок")
        (is (= (count transformed-result) 4) "Должно быть 4 объявления")
        (is (= (get-in transformed-result [0 :var-type]) "int") "Первое объявление должно быть типа int")))))

;; Тесты для управляющих конструкций
(deftest test-control-flow
  (testing "Парсинг простых управляющих конструкций"
    (let [input (create-input 
                 "int main() {"
                 "  if (x > 0) {"
                 "    return x;"
                 "  }"
                 "  while (y < 10) {"
                 "    y = y + 1;"
                 "  }"
                 "}")]
      (let [result (parser/parse input)]
        (is (not (:error result)) "Парсинг должен пройти без ошибок")
        (is (get-in result [:ast]) "AST должен быть сгенерирован")))))

;; Тесты для сложных выражений
(deftest test-expressions
  (testing "Парсинг арифметических выражений"
    (let [input (create-input 
                 "int calculate() {"
                 "  int result = (10 + 20) * 30 / 2;"
                 "  return result;"
                 "}")]
      (let [result (parser/parse input)]
        (is (not (:error result)) "Парсинг должен пройти без ошибок")
        (is (get-in result [:ast]) "AST должен быть сгенерирован")))))

;; Тесты для обработки ошибок
(deftest test-error-handling
  (testing "Парсинг некорректного кода"
    (let [input "int main( {"]
      (let [result (parser/parse input)]
        (is (:error result) "Должна быть сгенерирована ошибка парсинга")))))

;; Тесты для трансформации AST
(deftest test-ast-transformation
  (testing "Трансформация AST в семантическую структуру"
    (let [input (create-input 
                 "int example(int x) {"
                 "  return x + 1;"
                 "}")]
      (let [result (parser/parse-with-transform input)]
        (is (not (:error result)) "Парсинг должен пройти без ошибок")
        (is (= (count (:ast result)) 1) "Должна быть одна функция")
        (is (= (get-in result [:ast 0 :return-type]) "int") "Тип возвращаемого значения должен быть корректным")
        (is (get-in result [:ast 0 :body]) "Тело функции должно быть сгенерировано")))))