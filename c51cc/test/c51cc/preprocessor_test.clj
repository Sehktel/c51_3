(ns c51cc.preprocessor_test
  "Тесты для модуля предварительной обработки"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [c51cc.preprocessor :as preprocessor]
            [c51cc.logger :as log]))

(deftest test-remove-comments
  (testing "Удаление однострочных комментариев"
    (let [code "int main() { // Это комментарий\n    return 0; // Еще один комментарий\n}"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() { \n    return 0; \n}"))
      (log/debug "Тест удаления однострочных комментариев пройден")))
  
  (testing "Удаление многострочных комментариев"
    (let [code "int main() { /* Многострочный\n   комментарий */ return 0; }"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() { \n return 0; }"))
      (log/debug "Тест удаления многострочных комментариев пройден")))
  
  (testing "Удаление inline-комментариев"
    (let [code "int x = 10; // Инициализация переменной\n    int y = x + 5; // Операция сложения"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int x = 10; \n    int y = x + 5;"))
      (log/debug "Тест удаления inline-комментариев пройден")))
  
  (testing "Удаление смешанных комментариев"
    (let [code "/* Начальный комментарий */\nint main() { // Inline комментарий\n    /* Блочный комментарий */ return 0; /* Еще один */ }"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() { \n     return 0; }"))
      (log/debug "Тест удаления смешанных комментариев пройден")))
  
  (testing "Обработка кода без комментариев"
    (let [code "int main() { return 0; }"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() { return 0; }"))
      (log/debug "Тест обработки кода без комментариев пройден"))))

(deftest test-preprocess
  (testing "Полная предварительная обработка кода"
    (let [code "/* Заголовочный комментарий */\nint main() { // Inline комментарий\n    return 0; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "int main() { \n    return 0; }"))

      (log/debug "Тест полной предварительной обработки пройден")))) 