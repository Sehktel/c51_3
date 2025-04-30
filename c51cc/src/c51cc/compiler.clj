(ns c51cc.compiler
  "Основной модуль компилятора C51CC"
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [c51cc.logger :as log]
            [clojure.java.io :as io]
            [c51cc.preprocessor :as preprocessor]
            [c51cc.lexer :as lexer]
            [c51cc.parser :as parser]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :as stacktrace])
  (:import (java.io File)))

;; Определение спецификации командной строки
(def cli-options
  [["-h" "--help" "Показать справочную информацию"]
   ["-a" "--ast" "Вывести абстрактное синтаксическое дерево (AST)"]
   ["-i" "--IR" "Вывести промежуточное представление (Tree-Address Code)"]
   ["-l" "--lexer" "Вывести результаты лексического анализа"]
   ["-s" "--src" "Вывести исходный код после препроцессора (без комментариев)"]
   ["-p" "--preproc" "Вывести отладочную информацию препроцессора"]
   ["-S" "--ssa" "Вывести значения SSA (Static Single Assignment)"]
   ["-H" "--hir" "Вывести информацию о высокоуровневом промежуточном представлении"]
   ["-L" "--lir" "Вывести информацию о низкоуровневом промежуточном представлении"]
   ["-c" "--colortree" "Вывести значения цветового дерева и регистров"]
   ["-I" "--include PATH" "Путь для включаемых файлов"
    :validate [#(not (str/blank? %)) "Путь включения не может быть пустым"]]
   ["-i" "--ifile FILES" "Входные файлы"
    :parse-fn #(str/split % #",")
    :validate [#(not-empty %) "Должен быть указан хотя бы один входной файл"]]
   ["-o" "--ofile FILE" "Выходной файл"]
   ["-x" "--intelhex" "Вывести результат IntelHex"]
   ["-O" "--optimize LEVEL" "Установить уровень оптимизации"
    :parse-fn #(Integer/parseInt %)
    :validate [#(and (>= % 0) (<= % 3)) "Уровень оптимизации должен быть от 0 до 3"]]])

;; Утилиты для работы с файлами

(defn get-test-file
  "Получает путь к тестовому файлу из переменной окружения"
  []
  (or (System/getenv "TEST_FILE")
      (System/getProperty "test.file")
      (throw (ex-info "Не указан тестовый файл" 
                      {:message "Установите переменную окружения TEST_FILE"}))))

(defn- trace-include-file-search
  "Трассировка поиска включаемых файлов с расширенной диагностикой"
  [^String filename ^String base-path]
  (log/info (str "Поиск файла: " filename))
  (log/info (str "Базовый путь: " base-path))
  
  ;; Список потенциальных путей для поиска
  (let [search-paths [(io/file base-path filename)
                      (io/file (System/getProperty "user.dir") filename)
                      (io/file (System/getProperty "user.dir") "include" filename)
                      (io/file (System/getProperty "user.dir") "src" "include" filename)
                      (io/file (System/getProperty "user.dir") "c51code" filename)]]
   
    (log/info "Пути поиска:")
    (doseq [^File path search-paths]
      (log/info (str "Проверка пути: " (.getAbsolutePath path) 
                     " Существует: " (.exists path))))
    
    (first (filter #(.exists ^File %) search-paths))))

(defn read-file-content
  "Читает содержимое файла с расширенной диагностикой и обработкой ошибок"
  [^String file-path]
  (try 
    (log/trace (str "Попытка чтения файла: " file-path))
    
    (let [^File file (File. ^String file-path)]
      (when-not (.exists file)
        (throw (ex-info (str "Файл не существует: " file-path)
                        {:path file-path
                         :absolute-path (.getAbsolutePath file)
                         :file-details (bean file)})))
      
      (let [file-content (slurp file)]
        (log/debug (str "Успешно прочитан файл. Размер: " (count file-content) " байт"))
        file-content))
    (catch Exception e
      (log/error (str "Критическая ошибка чтения файла: " file-path 
                      ", Детали: " (.getMessage e)))
      (throw (ex-info (str "Ошибка чтения файла: " file-path)
                      {:file-path file-path
                       :error (.getMessage e)
                       :exception e})))))

(defn custom-read-include-file
  "Кастомная функция чтения включаемого файла с расширенной диагностикой"
  [^String filename ^String base-path]
  (log/info (str "Попытка чтения включаемого файла: " filename))
  
  (if-let [^File existing-file (trace-include-file-search filename base-path)]
    (do 
      (log/info (str "Найден файл: " (.getAbsolutePath existing-file)))
      (let [raw-content (slurp existing-file)
            cleaned-content (preprocessor/remove-comments raw-content)]
        (log/trace (str "Содержимое включаемого файла:\n" cleaned-content))
        cleaned-content))
    (throw (ex-info (str "Include file not found: " filename)
                    {:filename filename
                     :base-path base-path}))))

;; Функция для вывода справочной информации
(defn print-help [summary]
  (println "Использование: c51cc [опции]")
  (println "\nОпции:")
  (println summary))


(defn process-c-file
  "Полный цикл обработки файла с расширенной диагностикой"
  [& {:keys [source-path include-path] 
      :or {source-path (get-test-file)
           include-path (System/getProperty "user.dir")}}]
  (log/info "=== Начало полного цикла обработки файла ===")
  (log/set-debug-level! :DEBUG)
  
  (try
    (log/info "Stage 1: Подготовка и идентификация файлов")
    ;; Преобразование путей в строки
    (let [source-path (str source-path)
          include-path (str include-path)
          source-file (File. ^String source-path)]
      (log/info "Полный путь к исходному файлу:" (.getAbsolutePath source-file))
      (log/debug "Путь включения:" include-path)
      
      (log/info "Stage 2: Чтение и подготовка содержимого файлов")
      (let [source-content (read-file-content source-path)]
        (log/info "Размер исходного файла:" (count source-content) "байт")
        (log/trace "Содержимое исходного файла:\n" source-content)
        
        (log/info "Stage 3: Препроцессинг")
        (let [preprocessed-code (with-redefs [preprocessor/read-include-file custom-read-include-file]
                                 (preprocessor/preprocess source-content 
                                                         :base-path include-path))]
          (log/info "Результат препроцессинга:")
          (log/debug "Размер после препроцессинга:" (count preprocessed-code) "байт")
          (log/trace "Содержимое после препроцессинга:\n" preprocessed-code)
          
          (log/info "Stage 4: Токенизация")
          (let [tokens (lexer/tokenize preprocessed-code)]
            (log/info "Токенизация завершена")
            (log/debug "Количество токенов:" (count tokens))
            (println "=== Токены ===")
            (clojure.pprint/pprint tokens)
            
            (log/info "Stage 5: Построение AST")
            (let [ast (parser/parse tokens)]
              (log/info "Построение AST завершено")
              (log/debug "Сложность AST:" (count (:nodes ast)))
              (println "=== AST ===")
              (clojure.pprint/pprint ast)
              
              (log/info "=== Обработка файла успешно завершена ===")
              {:source-path source-path
               :include-path include-path
               :source-content source-content
               :preprocessed-code preprocessed-code
               :tokens tokens
               :ast ast})))))
    (catch Exception e
      (log/error "=== Критическая ошибка при обработке файла ===")
      (log/error "Сообщение:" (.getMessage e))
      (log/error "Стек вызовов:" (with-out-str (stacktrace/print-stack-trace e)))
      (throw (ex-info "Ошибка при обработке файла" 
                     {:source-path source-path
                      :include-path include-path
                      :error (.getMessage e)
                      :exception e})))))


;; Основная функция обработки аргументов
(defn -main
  "Точка входа компилятора с расширенной обработкой аргументов командной строки"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      ;; Обработка ошибок
      errors 
      (do 
        (log/error "Ошибки при парсинге аргументов:")
        (run! log/error errors)
        (System/exit 1))
      
      ;; Вывод справки
      (:help options)
      (do 
        (print-help summary)
        (System/exit 0))
      
      ;; Основная логика компилятора
      :else
      (do
        (log/info "Начало компиляции")
        (log/debug "Обработанные опции:" options)
        (log/debug "Аргументы:" arguments)
        
        ;; TODO: Добавить основную логику компилятора
        (println "Компиляция...")
        (log/info "Компиляция завершена")))))
