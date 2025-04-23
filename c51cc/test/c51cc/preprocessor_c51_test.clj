(ns c51cc.preprocessor_c51_test
  "Тестовый модуль для препроцессора с51"
  (:require [clojure.test :refer :all]
            [c51cc.preprocessor :as preprocessor]
            [c51cc.lexer :as lexer]
            [c51cc.parser :as parser]
            [c51cc.logger :as log]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- log-environment-variables 
  "Логирование переменных окружения для отладки"
  []
  (let [env-vars ["REG2051_H" "FILE_C" "INCLUDE_PATH"]]
    (doseq [var-name env-vars]
      (log/info (str "Переменная окружения " var-name ": " 
                     (or (System/getenv var-name) "НЕ УСТАНОВЛЕНА"))))))

(defn- trace-include-file-search
  "Трассировка поиска включаемых файлов"
  [filename base-path]
  (log/info (str "Поиск файла: " filename))
  (log/info (str "Базовый путь: " base-path))
  
  ;; Список потенциальных путей для поиска
  (let [search-paths [(io/file base-path filename)
                      (io/file (System/getProperty "user.dir") filename)
                      (io/file (System/getProperty "user.dir") "include" filename)
                      (io/file (System/getProperty "user.dir") "src" "include" filename)]]
    
    (log/info "Пути поиска:")
    (doseq [path search-paths]
      (log/info (str "Проверка пути: " (.getAbsolutePath path) 
                     " Существует: " (.exists path))))
    
    ;; Возвращаем первый существующий файл
    (first (filter #(.exists %) search-paths))))

(defn- get-header-file-path 
  "Получает путь к заголовочному файлу из переменной окружения"
  []
  (log-environment-variables)
  (let [header-file (System/getenv "REG2051_H")]
    (when-not header-file
      (log/error "Переменная окружения REG2051_H не установлена")
      (throw (ex-info "Переменная окружения REG2051_H не установлена" {})))
    
    ;; Дополнительная проверка существования файла
    (let [header-path (io/file header-file)]
      (when-not (.exists header-path)
        (log/error (str "Файл не существует: " header-file))
        (throw (ex-info (str "Файл не найден: " header-file)
                        {:path header-file
                         :absolute-path (.getAbsolutePath header-path)})))
      
      (log/debug (str "Получен путь к заголовочному файлу: " header-file))
      header-file)))

(defn- get-source-file-path 
  "Получает путь к исходному файлу из переменной окружения"
  []
  (let [source-file (System/getenv "FILE_C")]
    (when-not source-file
      (log/error "Переменная окружения FILE_C не установлена")
      (throw (ex-info "Переменная окружения FILE_C не установлена" {})))
    
    ;; Дополнительная проверка существования файла
    (let [source-path (io/file source-file)]
      (when-not (.exists source-path)
        (log/error (str "Файл не существует: " source-file))
        (throw (ex-info (str "Файл не найден: " source-file)
                        {:path source-file
                         :absolute-path (.getAbsolutePath source-path)})))
      
      (log/debug (str "Получен путь к исходному файлу: " source-file))
      source-file)))

(defn read-file-content
  "Читает содержимое файла с расширенной диагностикой"
  [file-path]
  (try 
    (log/trace (str "Попытка чтения файла: " file-path))
    
    ;; Расширенная проверка файла
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

(defn- custom-read-include-file
  "Кастомная функция чтения включаемого файла с расширенной диагностикой"
  [filename base-path]
  (log/info (str "Попытка чтения включаемого файла: " filename))
  (log/info (str "Базовый путь: " base-path))
  
  ;; Список потенциальных путей для поиска
  (let [search-paths [(io/file base-path filename)
                      (io/file (System/getProperty "user.dir") filename)
                      (io/file (System/getProperty "user.dir") "include" filename)
                      (io/file (System/getProperty "user.dir") "src" "include" filename)]]
    
    (log/info "Пути поиска:")
    (doseq [path search-paths]
      (log/info (str "Проверка пути: " (.getAbsolutePath path) 
                     " Существует: " (.exists path))))
    
    ;; Находим первый существующий файл
    (if-let [existing-file (first (filter #(.exists %) search-paths))]
      (do 
        (log/info (str "Найден файл: " (.getAbsolutePath existing-file)))
        (slurp existing-file))
      (throw (ex-info (str "Include file not found: " filename)
                      {:filename filename
                       :base-path base-path
                       :search-paths (map #(.getAbsolutePath %) search-paths)})))))

(defn process-c-file
  "Полный цикл обработки файла: 
    1. Чтение заголовочного и исходного файлов
    2. Объединение файлов
    3. Препроцессинг
    4. Токенизация
    5. Парсинг в AST
    
    Полный цикл обработки файла с расширенной диагностикой"
  []
  (log/info "Начало полного цикла обработки файла")
  
  ;; Логирование переменных окружения перед началом
  (log-environment-variables)
  
  (let [header-path (get-header-file-path)
        source-path (get-source-file-path)
        include-path (or (System/getenv "INCLUDE_PATH") 
                         (System/getProperty "user.dir"))
        
        ;; Чтение файлов с расширенной обработкой ошибок
        header-content (read-file-content header-path)
        source-content (read-file-content source-path)
        
        (log/info "Объединение файлов")
        ;; Объединение файлов
        merged-code (str header-content "\n" source-content)
        
        ;; Препроцессинг с кастомной функцией чтения включаемых файлов
        preprocessed-code (with-redefs [preprocessor/read-include-file custom-read-include-file]
                            (preprocessor/preprocess merged-code :base-path include-path))
        
        ;; Токенизация
        tokens (lexer/tokenize preprocessed-code)
        
        ;; Парсинг в AST
        ast (parser/parse tokens)]
    
    (log/info "Полный цикл обработки файла завершен")
    
    ;; Возвращаем результаты каждого этапа для возможного анализа
    {:header-path header-path
     :source-path source-path
     :merged-code merged-code
     :preprocessed-code preprocessed-code
     :tokens tokens
     :ast ast}))

(deftest test-c-file-processing
  (testing "Полный цикл обработки C-файла"
    (try 
      (let [result (process-c-file)]
        (is (map? result) "Результат должен быть map")
        (is (contains? result :header-path) "Должен содержать путь к заголовочному файлу")
        (is (contains? result :source-path) "Должен содержать путь к исходному файлу")
        (is (contains? result :merged-code) "Должен содержать объединенный код")
        (is (contains? result :preprocessed-code) "Должен содержать препроцессированный код")
        (is (contains? result :tokens) "Должен содержать токены")
        (is (contains? result :ast) "Должен содержать AST"))
      (catch Exception e
        (log/error "Критическая ошибка в тесте:" (.getMessage e))
        (is false (str "Тест не должен выбрасывать исключение: " (.getMessage e)))))))

(defn -main 
  "Точка входа для запуска обработки файла"
  [& args]
  (log/info "Запуск тестового модуля")
  (log/set-debug-level! :DEBUG)
  (try
    (let [result (process-c-file)]
      (log/info "Обработка файла завершена успешно")
      result)
    (catch Exception e
      (log/error (str "Ошибка при обработке файла: " (.getMessage e)))
      (throw e))))