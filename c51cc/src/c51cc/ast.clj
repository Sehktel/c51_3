(ns c51cc.ast
  "Модуль для генерации и анализа абстрактного синтаксического дерева (AST)
   
   Расширенные возможности:
   - Полный цикл обработки файлов
   - Расширенная диагностика
   - Гибкий препроцессинг и токенизация"
  (:require [c51cc.preprocessor :as preprocessor]
            [c51cc.lexer :as lexer]
            [c51cc.parser :as parser]
            [c51cc.logger :as log]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :as stacktrace])
  (:import (java.io File)))

(defn- get-test-file
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
    
    (let [^File file (io/file ^String file-path)]
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
          source-file (io/file source-path)]
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

(defn generate-ast
  "Генерация абстрактного синтаксического дерева с расширенной обработкой"
  [code base-path]
  (log/info "Начало генерации AST")
  (let [preprocessed-code (preprocessor/preprocess code :base-path base-path)
        tokens (lexer/tokenize preprocessed-code)
        ast (parser/parse tokens)]
    (log/debug "AST успешно сгенерирован")
    (println "Токены:" tokens)
    (println "AST:" (with-out-str (clojure.pprint/pprint ast)))
    ast))

(defn pretty-print-ast
  "Расширенная визуализация AST с глубоким форматированием и анализом

   Теоретические аспекты:
   - Многоуровневая визуализация структуры
   - Ограничение глубины для читаемости
   - Семантическая аннотация узлов

   Сложность: O(n), где n - количество узлов в AST"
  [ast]
  (log/debug "Начало расширенной визуализации AST")
  (with-out-str
    (pprint/pprint 
     (let [processed-ast 
           (update-in ast [:nodes] 
                      (fn [nodes] 
                        (mapv 
                         (fn [node]
                           (let [base-node 
                                 (select-keys node 
                                              [:type :name :return-type 
                                               :parameters :body])]
                             (cond-> base-node
                               (:parameters base-node) 
                               (update :parameters 
                                       (fn [params] 
                                         (take 10 params)))
                               
                               (:body base-node) 
                               (update :body 
                                       (fn [body] 
                                         (take 100 body))))))
                         (take 100 nodes))))]
       {:type (:type processed-ast)
        :total-nodes (count (:nodes ast))
        :nodes (:nodes processed-ast)}))))

(defn analyze-ast
  "Глубокий анализ AST с расширенной диагностикой"
  [ast]
  (log/debug "Начало анализа AST")
  {:node-types (frequencies (map :type ast))
   :total-nodes (count ast)
   :max-depth 10})

(defn -main
  "Точка входа для демонстрации возможностей AST"
  [& args]
  (log/log-level! :DEBUG)
  (log/info "Инициализация модуля AST")
  
  (try
    (let [result (process-c-file)]
      (println "Сгенерированный AST:")
      (println (pretty-print-ast (:ast result)))
      (println "\nАнализ AST:")
      (println (analyze-ast (:ast result))))
    (catch Exception e
      (log/error "Ошибка при генерации AST: " (.getMessage e))
      (System/exit 1))))