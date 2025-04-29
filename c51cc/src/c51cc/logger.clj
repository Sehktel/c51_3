(ns c51cc.logger
  "Многоуровневая система логирования с гибкой конфигурацией отладочных сообщений.
   Реализует иерархическую модель логирования с четырьмя уровнями детализации.")

(def ^:private debug-levels 
  "Определение иерархии уровней логирования с семантической градацией.
   Каждый последующий уровень включает в себя сообщения предыдущих уровней."
  {:SILENT  0   ; Полное молчание, никакие сообщения не выводятся
   :FATAL   1   ; Фатальные ошибки
   :ERROR   2   ; Только критические ошибки
   :WARNING 3   ; Предупреждения
   :INFO    4   ; Только важные информационные сообщения
   :DEBUG   5   ; Отладочные сообщения с дополнительным контекстом
   :TRACE   6}) ; Максимально детальная трассировка выполнения

(def ^:dynamic *current-debug-level* 
  "Динамическая переменная для текущего уровня логирования.
   По умолчанию установлен уровень INFO."
  :INFO)

(defn set-debug-level! 
  "Устанавливает глобальный уровень логирования.
   
   Параметры:
   - level: Ключевое слово, определяющее уровень логирования
   
   Возвращает установленный уровень логирования."
  [level]
  (alter-var-root #'*current-debug-level* (constantly level)))
  
(defn- can-log? 
  "Проверяет, можно ли логировать сообщение с учетом текущего уровня.
   
   Параметры:
   - message-level: Уровень конкретного сообщения
   
   Возвращает булево значение возможности логирования."
  [message-level]
  (<= (get debug-levels message-level)
      (get debug-levels *current-debug-level*)))

(defmacro log 
  "Макрос для условной записи логов с различными уровнями.
   
   Параметры:
   - level: Уровень логирования
   - & body: Тело сообщения для логирования
   
   Макрос выполняет логирование только если уровень сообщения 
   соответствует или ниже текущего глобального уровня."
  [level & body]
  `(when (can-log? ~level)
     (println (str (name ~level) ": " (apply str (map str ~body))))))

(defn trace 
  "Логирование с максимальной детализацией."
  [& messages]
  (when (can-log? :TRACE)
    (println (str "TRACE: " (apply str messages)))))

(defn debug 
  "Логирование отладочных сообщений."
  [& messages]
  (when (can-log? :DEBUG)
    (println (str "DEBUG: " (apply str messages)))))

(defn info 
  "Логирование информационных сообщений."
  [& messages]
  (when (can-log? :INFO)
    (println (str "INFO: " (apply str messages)))))

(defn warn 
  "Логирование предупреждений."
  [& messages]
  (when (can-log? :WARNING)
    (println (str "WARNING: " (apply str messages)))))

(defn error 
  "Логирование критических ошибок."
  [& messages]
  (when (can-log? :ERROR)
    (println (str "ERROR: " (apply str messages)))))

(defn fatal 
  "Логирование фатальных ошибок."
  [& messages]
  (when (can-log? :FATAL)
    (println (str "FATAL: " (apply str messages)))))
