(ns c51cc.preprocessor
  "Модуль предварительной обработки исходного кода C51"
  (:require [clojure.string :as str]
            [c51cc.logger :as log]))

;; Предварительное объявление для рекурсивной функции
(declare process-multiline-comments)

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
  ;; Состояние для отслеживания многострочных комментариев
  (let [state (atom {:in-multiline-comment false
                     :result-lines []})
        
        ;; Рекурсивная обработка многострочных комментариев в строке
        process-multiline-comments (fn [line]
                                   (if (str/includes? line "/*")
                                     (let [comment-start-idx (str/index-of line "/*")
                                           before-comment (subs line 0 comment-start-idx)
                                           rest-of-line (subs line comment-start-idx)]
                                       (if (str/includes? rest-of-line "*/")
                                         (let [comment-end-idx (str/index-of rest-of-line "*/")
                                               after-comment (subs rest-of-line (+ 2 comment-end-idx))]
                                           ;; Рекурсивно обрабатываем оставшуюся часть строки
                                           (str before-comment 
                                                (process-multiline-comments after-comment)))
                                         rest-of-line))  ; Комментарий не закрыт в этой строке
                                     line))  ; Нет начала комментария
        
        ;; Обработчик одной строки кода
        process-line (fn [line state]
                      (let [{:keys [in-multiline-comment result-lines]} @state]
                        (cond
                          ;; Внутри многострочного комментария
                          in-multiline-comment 
                          (if (str/includes? line "*/")
                            (let [comment-end-idx (str/index-of line "*/")
                                  after-comment (subs line (+ 2 comment-end-idx))]
                              (swap! state assoc 
                                    :in-multiline-comment false
                                    :result-lines (conj result-lines 
                                                      (process-multiline-comments after-comment))))  ; Обрабатываем оставшуюся часть
                            state)
                          
                          ;; Новая строка с возможными комментариями
                          :else 
                          (let [;; Сначала обрабатываем многострочные комментарии
                                line-without-multiline (process-multiline-comments line)
                                ;; Затем удаляем однострочные комментарии
                                line-without-comments (str/replace line-without-multiline #"//.*$" "")]
                            (swap! state assoc 
                                   :result-lines (conj result-lines line-without-comments))))))]
    
    ;; Обработка всех строк кода
    (doseq [line (str/split-lines code)]
      (process-line line state))
    
    ;; Финальная обработка результата
    (let [processed-code (str/join "\n" (:result-lines @state))]
      (log/trace "Длина исходного кода: " (count code)
                 ", Длина кода после удаления комментариев: " (count processed-code))
      
      ;; Очистка от лишних пробелов и переводов строк
      (-> processed-code
          (str/replace #"\n\s*\n" "\n")  ; Удаление пустых строк
          (str/trim)))))

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