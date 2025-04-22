(ns c51cc.preprocessor
  "Модуль предварительной обработки исходного кода C51"
  (:require [clojure.string :as str]
            [c51cc.logger :as log]))

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

(defn preprocess
  "Основная функция предварительной обработки кода

  Выполняет следующие шаги:
  1. Удаление комментариев
  2. Возможные будущие преобразования (например, обработка препроцессорных директив)

  Возвращает подготовленный к парсингу код"
  [code]
  (log/debug "Начало предварительной обработки исходного кода")
  (let [code-without-comments (remove-comments code)]
    (log/info "Предварительная обработка завершена")
    code-without-comments))