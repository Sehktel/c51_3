(ns c51cc.preprocessor
  "Модуль предварительной обработки исходного кода C51"
  (:require [clojure.string :as str]
            [c51cc.logger :as log]
            [clojure.java.io :as io]))

(declare process-includes)

(defn- process-line-comments
  "Обрабатывает все комментарии в одной строке.
   
   Параметры:
   - line: строка для обработки
   - in-comment?: находимся ли внутри многострочного комментария
   
   Возвращает:
   [результирующая-строка, все-еще-в-комментарии?]"
  [line in-comment?]
  (if in-comment?
    ;; Если мы внутри комментария, ищем его конец
    (if (str/includes? line "*/")
      [(str/trim (subs line (+ 2 (str/index-of line "*/")))) false]
      ["" true])
    ;; Если мы не внутри комментария, обрабатываем новые комментарии
    (loop [result ""
           current-line line
           in-multiline? false]
      (cond
        ;; Внутри многострочного комментария
        in-multiline?
        (if (str/includes? current-line "*/")
          (let [end-idx (+ 2 (str/index-of current-line "*/"))
                after-comment (subs current-line end-idx)]
            (recur result after-comment false))
          [result true])
        
        ;; Новый многострочный комментарий
        (str/includes? current-line "/*")
        (let [start-idx (str/index-of current-line "/*")
              before-comment (subs current-line 0 start-idx)]
          (if (str/includes? current-line "*/")
            (let [end-idx (+ 2 (str/index-of current-line "*/"))
                  after-comment (subs current-line end-idx)]
              (recur (str result before-comment) after-comment false))
            [(str result before-comment) true]))
        
        ;; Однострочный комментарий
        (str/includes? current-line "//")
        [(str result (str/trim (subs current-line 0 (str/index-of current-line "//")))) false]
        
        ;; Нет комментариев
        :else
        [(str result (str/trim current-line)) false]))))

(defn remove-comments
  "Удаляет комментарии из исходного кода C

  Поддерживает два типа комментариев:
  1. Однострочные комментарии (//)
  2. Многострочные комментарии (/* */)
  3. Inline-комментарии

  Алгоритм:
  - Последовательно обрабатывает входной текст
  - Удаляет комментарии, сохраняя структуру кода
  - Обеспечивает корректность обработки краевых случаев

  Сложность: O(n), где n - длина исходного кода"
  [code]
  (log/debug "Начало удаления комментариев из исходного кода")
  (let [state (atom {:in-multiline-comment false
                     :result-lines []})
        
        ;; Обработчик одной строки кода
        process-line (fn [line]
                      (let [[processed-line still-in-comment?] 
                            (process-line-comments line (:in-multiline-comment @state))]
                        (swap! state assoc :in-multiline-comment still-in-comment?)
                        (when (not (str/blank? processed-line))
                          (swap! state update :result-lines conj processed-line))))]
    
    ;; Обработка всех строк кода
    (doseq [line (str/split-lines code)]
      (process-line line))
    
    ;; Финальная обработка результата
    (let [processed-code (->> (:result-lines @state)
                             (str/join "\n"))]
      (log/trace "Длина исходного кода: " (count code)
                 ", Длина кода после удаления комментариев: " (count processed-code))
      
      ;; Очистка от лишних пробелов и переводов строк
      (-> processed-code
          (str/replace #"\n\s*\n" "\n")  ; Удаление пустых строк
          str/trim))))

(defn- read-include-file
  "Читает содержимое файла для включения.
   
   Параметры:
   - filename: имя файла для включения (без < > или \" \")
   - base-path: базовый путь для поиска файла
   
   Возвращает содержимое файла или выбрасывает исключение, если файл не найден"
  [filename base-path]
  (let [file (io/file base-path filename)]
    (if (.exists file)
      (do
        (log/debug "Чтение файла для включения:" filename)
        (slurp file))
      (throw (ex-info (str "Include file not found: " filename)
                     {:filename filename
                      :base-path base-path})))))

(defn- extract-filename
  "Извлекает имя файла из директивы #include.
   
   Параметры:
   - include-str: строка вида <file.h> или \"file.h\"
   
   Возвращает имя файла без < > или \" \""
  [include-str]
  (let [trimmed (str/trim include-str)]
    (cond
      (and (str/starts-with? trimmed "<")
           (str/ends-with? trimmed ">"))
      (subs trimmed 1 (dec (count trimmed)))
      
      (and (str/starts-with? trimmed "\"")
           (str/ends-with? trimmed "\""))
      (subs trimmed 1 (dec (count trimmed)))
      
      :else
      (throw (ex-info "Invalid include format"
                     {:include-str include-str})))))

(defn process-includes
  "Обрабатывает директивы #include в коде.
   
   Параметры:
   - code: исходный код
   - base-path: базовый путь для поиска включаемых файлов
   
   Возвращает код с обработанными директивами include"
  [code base-path]
  (log/debug "Начало обработки директив #include")
  (let [include-pattern #"#include\s+[<\"][^>\"]*[>\"]"]
    (loop [current-code code
           processed-files #{}]
      (if-let [include-match (re-find include-pattern current-code)]
        (let [filename (extract-filename (subs include-match 8))
              _ (when (contains? processed-files filename)
                  (log/warn "Циклическое включение файла:" filename))
              file-content (if (contains? processed-files filename)
                           ""  ; Пропускаем повторное включение
                           (read-include-file filename base-path))
              new-code (str/replace-first current-code include-match file-content)]
          (recur new-code (conj processed-files filename)))
        current-code))))

(defn- process-defines
  "Обрабатывает директивы #define в коде.
   
   Параметры:
   - code: исходный код
   
   Возвращает код с обработанными директивами #define"
  [code]
  (log/debug "Начало обработки директив #define")
  (let [define-pattern #"#define\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+(.+)"]
    (loop [current-code code
           defines {}
           iteration 0]
      (if (> iteration 10)  ; Предотвращение бесконечного цикла
        current-code
        (if-let [[full-match name value] (re-find define-pattern current-code)]
          (let [trimmed-value (str/trim value)
                ;; Заменяем вложенные определения
                resolved-value (reduce-kv 
                                (fn [v k replacement]
                                  (str/replace v (re-pattern (str "\\b" k "\\b")) 
                                               (str "(" replacement ")")))
                                trimmed-value
                                defines)
                new-code (str/replace-first current-code full-match "")]
            (recur new-code 
                   (assoc defines name resolved-value)
                   (inc iteration)))
          ;; Финальная замена всех определений
          (reduce (fn [code [name value]]
                    (str/replace code 
                                 (re-pattern (str "\\b" name "\\b")) 
                                 (str "(" value ")")))
                  current-code
                  defines))))))

(defn preprocess
  "Основная функция предварительной обработки кода

  Выполняет следующие шаги:
  1. Удаление комментариев
  2. Обработка директив #include
  3. Обработка директив #define
  4. Возможные будущие преобразования

  Возвращает подготовленный к парсингу код"
  [code & {:keys [base-path] :or {base-path "."}}]
  (log/debug "Начало предварительной обработки исходного кода")
  (-> code
      remove-comments
      (process-includes base-path)
      process-defines))