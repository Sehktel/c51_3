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
  (let [env-vars ["FILE_C" "INCLUDE_PATH"]]
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
  "Кастомная функция чтения включаемого файла с расширенной диагностикой и удалением комментариев"
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
        ;; Читаем файл и удаляем комментарии с помощью препроцессора
        (let [raw-content (slurp existing-file)
              cleaned-content (preprocessor/remove-comments raw-content)]
          cleaned-content))
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
  (log/info "Stage 1: Подготовка и идентификация файлов")

  (log/set-debug-level! :DEBUG)
  (log/info (get-source-file-path))  

  (let [source-path (get-source-file-path)
        include-path (or (System/getenv "INCLUDE_PATH") 
                         (System/getProperty "user.dir"))]
    
    ;; 1.a Печать полного пути к исходному файлу
    (log/info "Полный путь к исходному файлу:" (.getAbsolutePath (io/file source-path)))
    (log/debug "Путь включения:" include-path)
    
    ;; Стадия 2: Чтение и подготовка содержимого файлов
    (log/info "Stage 2: Чтение и подготовка содержимого файлов")
    (let [source-content (read-file-content source-path)
          
          ;; 2.1 Печать содержимого исходного файла
          _ (do 
              (log/info "Размер исходного файла:" (count source-content) "байт")
              (log/trace "Содержимое исходного файла:\n" source-content))
          
          ;; Препроцессинг с кастомной функцией чтения включаемых файлов
          preprocessed-code (with-redefs [preprocessor/read-include-file 
                                          (fn [filename base-path]
                                            (let [include-file (custom-read-include-file filename base-path)]
                                              ;; 1.b Печать полного пути к включаемому заголовочному файлу
                                              (log/info "Полный путь к включаемому файлу:" 
                                                       (-> (io/file base-path filename) 
                                                           .getAbsolutePath))
                                              ;; 2.2 Печать содержимого заголовочного файла
                                              (log/trace "Содержимое включаемого файла:\n" include-file)
                                              include-file))]
            
            ;; 2.3 Печать объединенного содержимого
            (log/info "2.3 Печать объединенного содержимого")

            (log/debug "Объединение файлов")
            
            ;; 2.4 Препроцессинг и очистка
            (log/info "2.4 Препроцессинг и очистка")
            (preprocessor/preprocess source-content :base-path include-path))]
      
      (log/info "Результат препроцессинга:")
      (log/debug "Размер после препроцессинга:" (count preprocessed-code) "байт")
      
      (log/log-level! :TRACE)
      (log/trace "Содержимое после препроцессинга:\n" preprocessed-code)
      (log/log-level! :DEBUG)

      ;; Стадия 3: Токенизация
      (log/info "Stage 3: Токенизация")
      (let [tokens (lexer/tokenize preprocessed-code)]
        (log/info "Токенизация завершена")
        (log/debug "Количество токенов:" (count tokens))
        (log/trace "Токены:\n" tokens)
        
        ;; Стадия 4: Построение AST
        (log/info "Stage 4: Построение AST")
        (let [ast (parser/parse tokens)]
          (log/info "Построение AST завершено")
          (log/debug "Сложность AST:" (count ast))
          (log/trace "Абстрактное синтаксическое дерево:\n" ast)
          
          ;; Возвращаем результаты с полной информацией
          {:source-path source-path
           :include-path include-path
           :source-content source-content
           :preprocessed-code preprocessed-code
           :tokens tokens
           :ast ast})))))

(deftest test-c-file-processing
  (testing "Полный цикл обработки C-файла"
    (try 
      (let [result (process-c-file)]
        (is (map? result) "Результат должен быть map")
        (is (contains? result :source-path) "Должен содержать путь к исходному файлу")
        (is (contains? result :include-path) "Должен содержать путь включения")
        (is (contains? result :source-content) "Должен содержать содержимое исходного файла")
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