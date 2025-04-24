(ns c51cc.ast_test
  "Модуль тестирования генерации и анализа абстрактного синтаксического дерева (AST)"
  (:require [clojure.test :refer :all]
            [c51cc.ast :as ast]
            [c51cc.logger :as log]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.stacktrace :as stacktrace]))

;; Константы для конфигурации тестирования
(def ^:private default-test-file-env "TEST_FILE")
(def ^:private default-include-path-env "INCLUDE_PATH")

(defn- print-env-vars
  "Печатает значения переменных окружения для отладки"
  []
  (let [env-vars [default-test-file-env 
                  default-include-path-env 
                  "TEST_FILE" 
                  "FILE_C" 
                  "INCLUDE_PATH" 
                  ]]
    (log/info "=== Переменные окружения ===")
    (doseq [var-name env-vars]
      (log/debug (format "%-20s: %s" 
                       var-name 
                       (or (System/getenv var-name) 
                           (System/getProperty var-name) 
                           "НЕ УСТАНОВЛЕНА"))))
    (log/info "=== Системные свойства ===")
    (doseq [[prop-name prop-value] (sort (System/getProperties))]
      (when (re-find #"(user|file|path|dir)" (str prop-name))
        (log/debug (format "%-40s: %s" prop-name prop-value))))
    (log/info "==========================")))

(defn- get-env-var
  "Получает значение переменной окружения с возможностью указания значения по умолчанию"
  ([var-name] (get-env-var var-name nil))
  ([var-name default-value]
   (or (System/getenv var-name)
       (System/getProperty (str "env." var-name))
       default-value)))

(defn- log-environment-variables 
  "Логирование переменных окружения для отладки"
  []
  (let [env-vars ["FILE_C" "INCLUDE_PATH" "TEST_FILE"]]
    (log/info "=== Переменные окружения ===")
    (doseq [var-name env-vars]
      (log/info (str "Переменная окружения " var-name ": " 
                     (or (System/getenv var-name) 
                         (System/getProperty var-name) 
                         "НЕ УСТАНОВЛЕНА"))))
    
    (log/info "=== Системные свойства ===")
    (doseq [[prop-name prop-value] 
            (->> (System/getProperties)
                 (into {})
                 (filter (fn [[k _]] (re-find #"(user|file|path|dir)" (str k))))
                 (sort-by first))]
      (log/info (format "%-40s: %s" prop-name prop-value)))
    
    (log/info "==========================")))

(defn- get-test-file
  "Получает путь к тестовому файлу из переменных окружения
    
   Приоритет источников:
   1. Переменная окружения TEST_FILE
   2. Переменная окружения FILE_C
   3. Системное свойство test.file
   4. Значение по умолчанию"
  []
  (log/info "Stage 1: Определение тестового файла")
  (let [env-vars ["TEST_FILE" "FILE_C" "test.file"]
        test-file-candidates (keep #(get-env-var % nil) env-vars)
        default-file "c51code/simple_example.c"
        
        ;; Выбираем первый существующий файл или используем значение по умолчанию
        test-file (or 
                   (first (filter #(.exists (io/file %)) test-file-candidates))
                   default-file)]
    
    (log/info "Кандидаты тестового файла:" (pr-str test-file-candidates))
    (log/info "Выбран тестовый файл:" test-file)
    
    ;; Проверка существования файла с более подробной диагностикой
    (let [file (io/file test-file)]
      (when-not (.exists file)
        (log/error "Тестовый файл не найден:" test-file)
        (log/error "Текущий рабочий каталог:" (System/getProperty "user.dir"))
        (log/error "Абсолютный путь файла:" (.getAbsolutePath file))
        (throw (ex-info "Тестовый файл не найден" 
                       {:file test-file
                        :current-dir (System/getProperty "user.dir")
                        :absolute-path (.getAbsolutePath file)
                        :candidates test-file-candidates}))))
    test-file))

(defn- get-include-path
  "Получает путь включения из переменных окружения"
  []
  (log/info "Stage 2: Определение пути включения")
  (let [include-path (get-env-var default-include-path-env "c51code")]
    (log/info "Используется путь включения:" include-path)
    include-path))

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
  "#include <reg51.h>

  void main(void) { ; }
")

(def ^:private complex-c-code
  "Более сложный тестовый код с различными конструкциями"
  "#include <reg51.h>
  #define A 5
  #define B 10
  void main(void) {
    char C;
    C = A + B;
  }")

(deftest test-generate-ast
  "Тест генерации абстрактного синтаксического дерева"
  (testing "Генерация AST из переменной окружения"
    (log/info "=== Начало теста генерации AST ===")
    (log-environment-variables)
    (try
      (let [test-file (get-test-file)
            include-path (get-include-path)
            _ (log/info "Stage 3: Генерация AST")
            _ (log/info "Тестовый файл:" test-file)
            _ (log/info "Путь включения:" include-path)
            result (ast/process-c-file 
                    :source-path test-file 
                    :include-path include-path)]
        
        (log/info "Результат обработки файла:")
        (log/info "Путь к исходному файлу:" (:source-path result))
        (log/info "Путь включения:" (:include-path result))
        (log/info "Размер исходного содержимого:" (count (:source-content result)) "байт")
        (log/info "Размер препроцессированного кода:" (count (:preprocessed-code result)) "байт")
        (log/info "Количество токенов:" (count (:tokens result)))
        (log/info "Количество узлов AST:" (count (:ast result)))
        
        (is (map? result) "Результат должен быть картой")
        (is (contains? result :ast) "Результат должен содержать AST")
        (is (pos? (count (:ast result))) "AST не должен быть пустым")
        (log/info "=== Тест успешно завершен ==="))
      (catch Exception e
        (log/error "Критическая ошибка в тесте:" (.getMessage e))
        (log/error "Стек вызовов:" (with-out-str (stacktrace/print-stack-trace e)))
        (throw e)))))

(deftest test-pretty-print-ast
  "Тест визуализации абстрактного синтаксического дерева"
  (testing "Визуализация AST из переменной окружения"
    (let [test-file (get-test-file)
          include-path (get-include-path)
          result (ast/process-c-file 
                  :source-path test-file 
                  :include-path include-path)
          pretty-print (ast/pretty-print-ast (:ast result))]
      
      ;; Логирование результатов pretty-print
      (log/info "=== Результат Pretty Print AST ===")
      (log/info "Длина строки:" (count pretty-print))
      (log/info "Первые 500 символов:\n" (subs pretty-print 0 (min 500 (count pretty-print))))
      
      ;; Проверки
      (is (string? pretty-print) "Визуализация должна быть строкой")
      (is (pos? (count pretty-print)) "Визуализация не должна быть пустой")
      
      ;; Дополнительные проверки структуры
      (let [parsed-print (read-string pretty-print)]
        (is (map? parsed-print) "Pretty print должен быть картой")
        (is (contains? parsed-print :type) "Карта должна содержать ключ :type")
        (is (contains? parsed-print :total-nodes) "Карта должна содержать ключ :total-nodes")
        (is (contains? parsed-print :nodes) "Карта должна содержать ключ :nodes")
        (is (< (count (:nodes parsed-print)) 11) "Должно быть не более 10 узлов в AST")))))

(defn -main
  "Точка входа для запуска тестов AST"
  [& args]
  (log/log-level! :DEBUG)
  (log/debug "Начало тестирования AST")
  ;; Печать переменных окружения перед запуском тестов
  (print-env-vars)
  
  (run-tests 'c51cc.ast_test))