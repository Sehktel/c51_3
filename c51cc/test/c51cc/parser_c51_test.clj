(ns c51cc.parser_c51_test
  "Тесты для парсинга реальных C51 файлов"
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [c51cc.logger :as log]
            [c51cc.lexer :as lexer]
            [c51cc.parser :as parser]
            [c51cc.preprocessor :as preprocessor]
            [clojure.stacktrace :as stacktrace])
  (:import (java.io File)))

(defn- list-c-files 
  "Получение списка .c файлов в указанной директории"
  [dir-path]
  (let [dir (io/file dir-path)]
    (filter #(and (.isFile %) 
                  (.endsWith (.getName %) ".c"))
            (file-seq dir))))

(defn- read-file-content 
  "Чтение содержимого файла с обработкой кодировки"
  [^File file]
  (slurp file :encoding "UTF-8"))

(defn- parse-c-file 
  "Парсинг C-файла через лексер и парсер с расширенной диагностикой"
  [file]
  (log/debug "Парсинг файла:" (.getName file))
  (log/set-debug-level! :TRACE)
  (log/trace "Очищенное содержимое:" (preprocessor/remove-comments (read-file-content file)))
  (log/set-debug-level! :INFO)
  (let [content (read-file-content file)
        cleaned-content (preprocessor/remove-comments content)
        tokens  (lexer/tokenize cleaned-content)
        _       (log/debug "Количество токенов:" (count tokens))
        result  (parser/parse tokens)]
    (log/info "Файл успешно распарсен:" (.getName file))
    result))

(defn- validate-function-node
  "Валидация узла функции с учетом специфики файла"
  [func file-name]
  (cond 
    (= file-name "interrupt_handler.c")
    (do 
      (is (= (:name func) "Timer0_ISR") "Должна быть функция Timer0_ISR")
      (is (= (:return-type func) "void") "Тип возврата должен быть void")
      (is (= (get-in func [:parameters 0]) {:type "void" :name nil}) "Параметры должны быть void"))
    
    (= file-name "multiple_functions.c")
    (let [valid-names #{"init" "main"}]
      (is (contains? valid-names (:name func)) 
          (str "Функция должна быть из списка: " valid-names)))
    
    (= file-name "complex_interrupt.c")
    (do
      (is (= (:name func) "Complex_Interrupt") "Должна быть функция Complex_Interrupt")
      (is (= (:return-type func) "void") "Тип возврата должен быть void"))))

(defn- analyze-parse-result 
  "Глубокий анализ результата парсинга"
  [parse-result file-name]
  (testing (str "Анализ структуры для " file-name)
    (is (= (:type parse-result) :program) "Результат парсинга должен быть программой")
    (is (sequential? (:nodes parse-result)) "Узлы программы должны быть последовательностью")
    (is (pos? (count (:nodes parse-result))) "Должен быть хотя бы один узел")
    
    ;; Проверка типов узлов
    (let [node-types (map :type (:nodes parse-result))]
      (is (every? #{:function-declaration} node-types) 
          "Все узлы должны быть объявлениями функций"))
    
    ;; Валидация каждой функции
    (doseq [func (:nodes parse-result)]
      (validate-function-node func file-name))))

(deftest test-parse-c51-files
  (testing "Парсинг всех .c файлов в директории тестов"
    (log/set-debug-level! :DEBUG)
    (let [test-dir "test/c51code"
          c-files  (list-c-files test-dir)]
      (if (empty? c-files)
        (log/warn "В директории тестов нет .c файлов")
        (doseq [file c-files]
          (testing (str "Парсинг файла: " (.getName file))
            (try 
              (let [parse-result (parse-c-file file)]
                (analyze-parse-result parse-result (.getName file)))
              (catch Exception e
                (let [error-msg (format "Ошибка парсинга файла %s: %s\nПолный стек-трейс: %s" 
                                        (.getName file) 
                                        (.getMessage e) 
                                        (with-out-str (stacktrace/print-stack-trace e)))]
                  (log/error error-msg)
                  (is false error-msg))))))))))

;; Функция для создания тестового файла, если его нет
(defn- create-sample-c51-file 
  "Создание примера C51 файла для тестирования"
  []
  (let [sample-code "void P14 (void) interrupt 0 { while(1){ P1 = 0x04; } }"
        test-file (io/file "test/c51code/sample.c")]
    (when-not (.exists test-file)
      (spit test-file sample-code)
      (log/info "Создан тестовый файл:" (.getPath test-file)))))

;; Инициализация тестовых данных перед запуском
(create-sample-c51-file)

(defn parse-and-print-file
  "Функция для ручной проверки парсинга конкретного файла"
  [file-path]
  (let [file (io/file file-path)]
    (when-not (.exists file)
      (throw (ex-info "Файл не существует" {:path file-path})))
    
    (log/set-debug-level! :DEBUG)
    (println "Парсинг файла:" file-path)
    
    (try 
      (let [content (slurp file :encoding "UTF-8")
            tokens  (lexer/tokenize content)
            _       (println "Токены:")
            _       (pprint/pprint tokens)
            _       (println "\nКоличество токенов:" (count tokens))
            result  (parser/parse tokens)]
        
        (println "\nРезультат парсинга:")
        (pprint/pprint result)
        
        ;; Дополнительная валидация
        (is (= (:type result) :program) "Результат должен быть программой")
        (is (pos? (count (:nodes result))) "Должен быть хотя бы один узел")
        
        result)
      (catch Exception e
        (println "Ошибка при парсинге:")
        (stacktrace/print-stack-trace e)
        (throw e)))))

(defn- manual-parse-test 
  "Ручной тест для парсинга указанного файла"
  [file-path]
  (log/set-debug-level! :TRACE)
  (testing (str "Ручной парсинг файла: " file-path)
    (parse-and-print-file file-path))
  (log/set-debug-level! :INFO)
  )

(deftest ^:manual test-manual-parse
  (log/set-debug-level! :INFO)
  
  ;; More robust property retrieval
  (let [system-test-file (or (System/getProperty "test.file")
                             (System/getenv "TEST_FILE")
                             "test/c51code/interrupt_handler.c")
        _ (println "DEBUG: Системное свойство test.file:" system-test-file)
        ;; _ (println "DEBUG: Все системные свойства:" (System/getProperties))
        ;; _ (println "DEBUG: Переменные окружения:" (System/getenv))
        ]
    
    (log/info "Ручной парсинг файла: " system-test-file)
    (manual-parse-test system-test-file))) 