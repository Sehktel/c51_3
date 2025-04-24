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
            [clojure.pprint :as pprint]))

(defn- trace-include-file-search
  "Трассировка поиска включаемых файлов с расширенной диагностикой"
  [filename base-path]
  (log/info (str "Поиск файла: " filename))
  (log/info (str "Базовый путь: " base-path))
  
  (let [search-paths [(io/file base-path filename)
                      (io/file (System/getProperty "user.dir") filename)
                      (io/file (System/getProperty "user.dir") "include" filename)
                      (io/file (System/getProperty "user.dir") "src" "include" filename)]]
    
    (log/info "Пути поиска:")
    (doseq [path search-paths]
      (log/info (str "Проверка пути: " (.getAbsolutePath path) 
                     " Существует: " (.exists path))))
    
    (first (filter #(.exists %) search-paths))))

(defn read-file-content
  "Читает содержимое файла с расширенной диагностикой и обработкой ошибок"
  [file-path]
  (try 
    (log/trace (str "Попытка чтения файла: " file-path))
    
    (let [file (io/file file-path)]
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
  [filename base-path]
  (log/info (str "Попытка чтения включаемого файла: " filename))
  
  (if-let [existing-file (trace-include-file-search filename base-path)]
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
  [source-path & {:keys [include-path] 
                  :or {include-path (System/getProperty "user.dir")}}]
  (log/info "Начало полного цикла обработки файла")
  (log/set-debug-level! :DEBUG)
  
  (log/info "Полный путь к исходному файлу:" (.getAbsolutePath (io/file source-path)))
  (log/debug "Путь включения:" include-path)
  
  (let [source-content (read-file-content source-path)
        
        _ (log/info "Размер исходного файла:" (count source-content) "байт")
        _ (log/trace "Содержимое исходного файла:\n" source-content)
        
        preprocessed-code (with-redefs [preprocessor/read-include-file custom-read-include-file]
                             (preprocessor/preprocess source-content 
                                                     :base-path include-path))]
    
    (log/info "Результат препроцессинга:")
    (log/debug "Размер после препроцессинга:" (count preprocessed-code) "байт")
    (log/trace "Содержимое после препроцессинга:\n" preprocessed-code)
    
    (let [tokens (lexer/tokenize preprocessed-code)]
      (log/info "Токенизация завершена")
      (log/debug "Количество токенов:" (count tokens))
      (log/trace "Токены:\n" tokens)
      
      (let [ast (parser/parse tokens)]
        (log/info "Построение AST завершено")
        (log/debug "Сложность AST:" (count ast))
        (log/trace "Абстрактное синтаксическое дерево:\n" ast)
        
        {:source-path source-path
         :include-path include-path
         :source-content source-content
         :preprocessed-code preprocessed-code
         :tokens tokens
         :ast ast}))))

(defn generate-ast
  "Генерация абстрактного синтаксического дерева с расширенной обработкой"
  [code base-path]
  (log/info "Начало генерации AST")
  (let [preprocessed-code (preprocessor/preprocess code :base-path base-path)
        tokens (lexer/tokenize preprocessed-code)
        ast (parser/parse tokens)]
    (log/debug "AST успешно сгенерирован")
    ast))

(defn pretty-print-ast
  "Красивая визуализация AST с расширенным форматированием"
  [ast]
  (log/debug "Начало визуализации AST")
  (with-out-str
    (pprint/pprint 
     (update-in ast [:body] 
                (fn [body] (take 10 body))))))

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
  (log/set-level! :DEBUG)
  (log/info "Инициализация модуля AST")
  
  (when (seq args)
    (let [file-path (first args)
          base-path (or (second args) ".")]
      (try
        (let [result (process-c-file file-path :include-path base-path)]
          (println "Сгенерированный AST:")
          (println (pretty-print-ast (:ast result)))
          (println "\nАнализ AST:")
          (println (analyze-ast (:ast result))))
        (catch Exception e
          (log/error "Ошибка при генерации AST: " (.getMessage e))
          (System/exit 1))))))