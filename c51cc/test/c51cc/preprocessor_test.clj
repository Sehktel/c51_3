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
      (is (= processed-code "int main() {\nreturn 0;\n}"))
      (log/debug "Тест удаления однострочных комментариев пройден")))
  
  (testing "Удаление многострочных комментариев"
    (let [code "int main() { /* Многострочный\n   комментарий */ return 0; }"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() { \nreturn 0; }"))
      (log/debug "Тест удаления многострочных комментариев пройден")))
  
  (testing "Удаление inline-комментариев"
    (let [code "int x = 10; // Инициализация переменной\n    int y = x + 5; // Операция сложения"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int x = 10;\nint y = x + 5;"))
      (log/debug "Тест удаления inline-комментариев пройден")))
  
  (testing "Удаление смешанных комментариев"
    (let [code "/* Начальный комментарий */\nint main() { // Inline комментарий\n    /* Блочный комментарий */ return 0; /* Еще один */ }"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() {\n     return 0; }"))
      (log/debug "Тест удаления смешанных комментариев пройден")))
  
  (testing "Обработка кода без комментариев"
    (let [code "int main() { return 0; }"
          processed-code (preprocessor/remove-comments code)]
      (is (= processed-code "int main() { return 0; }"))
      (log/debug "Тест обработки кода без комментариев пройден"))))

(deftest test-process-defines
  (testing "Простая замена константы"
    (let [code "#define BIT_2 (1 << 2)\nint main() { P1 &= ~BIT_2; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "\nint main() { P1 &= ~(1 << 2); }"))
      (log/debug "Тест простой замены константы пройден")))
  
  (testing "Несколько #define директив"
    (let [code "#define BIT_2 (1 << 2)\n#define BIT_3 (1 << 3)\nint main() { P1 &= ~(BIT_2 | BIT_3); }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "\n\nint main() { P1 &= ~((1 << 2) | (1 << 3)); }"))
      (log/debug "Тест нескольких #define директив пройден")))
  
  (testing "Сложные выражения в #define"
    (let [code "#define MAX_VALUE 255\n#define SHIFT_VALUE (MAX_VALUE >> 1)\nint main() { int x = SHIFT_VALUE; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "\n\nint main() { int x = (255 >> 1); }"))
      (log/debug "Тест сложных выражений в #define пройден")))
  
  (testing "Повторяющиеся #define"
    (let [code "#define BIT_2 (1 << 2)\n#define BIT_2 (1 << 2)\nint main() { P1 &= ~BIT_2; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "\n\nint main() { P1 &= ~(1 << 2); }"))
      (log/debug "Тест повторяющихся #define директив пройден")))
  
  (testing "Вложенные #define"
    (let [code "#define BASE_SHIFT 1\n#define BIT_2 (BASE_SHIFT << 2)\nint main() { P1 &= ~BIT_2; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "\n\nint main() { P1 &= ~(1 << 2); }"))
      (log/debug "Тест вложенных #define директив пройден"))))

(deftest test-preprocess
  (testing "Полная предварительная обработка кода"
    (let [code "/* Заголовочный комментарий */\nint main() { // Inline комментарий\n    return 0; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "int main() {\nreturn 0; }"))

      (log/debug "Тест полной предварительной обработки пройден")))

  (testing "Предварительная обработка с #define"
    (let [code "#define BIT_2 (1 << 2)\n/* Комментарий */\nint main() { P1 &= ~BIT_2; }"
          processed-code (preprocessor/preprocess code)]
      (is (= processed-code "\nint main() { P1 &= ~(1 << 2); }"))
      (log/debug "Тест предварительной обработки с #define пройден")))) 