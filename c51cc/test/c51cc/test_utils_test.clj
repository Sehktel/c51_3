(ns c51cc.test-utils-test
  (:require [clojure.test :refer :all]
            [c51cc.test-utils :as sut]))

(deftest test-parse-parameters-example
  (testing "Пример использования test-parse-parameters"
    (let [input "int a, char b"
          result (sut/test-parse-parameters input)]
      ;; Здесь должна быть проверка ожидаемого результата
      (is (some? result)))))

(deftest test-operators-example
  (testing "Пример проверки операторов"
    (is (sut/test-is-binary-operator? "+"))
    (is (sut/test-is-unary-operator? "!"))
    (is (sut/test-is-bitwise-operator? "&"))))

(deftest test-parse-expression-example
  (testing "Пример парсинга выражений"
    (let [input "a + b * 2"
          result (sut/test-parse-expression input)]
      ;; Здесь должна быть проверка ожидаемого результата
      (is (some? result))))) 