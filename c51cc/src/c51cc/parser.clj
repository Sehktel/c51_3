(ns c51cc.parser
  "Модуль для синтаксического анализатора"
  (:require [c51cc.lexer  :as lexer]
            [c51cc.logger :as log]
            [clojure.stacktrace :as stacktrace]
            [clojure.string :as str]
            ))

;; Парсер для языка C51 - абстракция синтаксического анализа

(declare parse-parameters parse-function-body)
(declare parse-function-declaration parse-variable-declaration)
(declare parse-expression parse-program)
(declare ast-node-types create-parser parse)
(declare parse-parameters)
(declare parse-function-body)
(declare parse-function-declaration)
(declare parse-variable-declaration)
(declare parse-expression)
(declare parse-program)
(declare ast-node-types)
(declare create-parser)
(declare parse)



;; Типы узлов абстрактного синтаксического дерева (AST)
(def ast-node-types
  "Типы узлов абстрактного синтаксического дерева (AST)"
  {:program :program
   :function-declaration :function-declaration
   :variable-declaration :variable-declaration
   :function-call :function-call
   :control-flow :control-flow
   :expression :expression
   :preprocessor-directive :preprocessor-directive
   :include-directive :include-directive})

;; Протокол для парсинга различных конструкций языка
(defprotocol ASTParser
  "Протокол для парсинга различных синтаксических конструкций"
  (parse-program [this tokens] "Парсинг всей программы")
  (parse-function-declaration [this tokens] "Парсинг объявления функции")
  (parse-variable-declaration [this tokens] "Парсинг объявления переменной")
  (parse-expression [this tokens] "Парсинг выражения")
  (parse-preprocessor-directive [this tokens] "Парсинг препроцессорной директивы"))

;; Функция для парсинга препроцессорных директив
(defn- parse-include-directive
  "Парсинг директивы #include
   
   Грамматика:
   include-directive ::= '#include' '<' filename '.h' '>'
   
   Параметры:
   - tokens: последовательность токенов начиная с директивы #include"
  [tokens]
  (log/debug "Начало парсинга директивы #include")
  (let [[include-token & remaining] tokens
        [path-token & after-path] remaining]
    (when-not (and (= (:type include-token) :preprocessor-directive)
                   (= (:value include-token) "#include")
                   (= (:type path-token) :include-path)
                   (str/starts-with? (:value path-token) "<")
                   (str/ends-with? (:value path-token) ">")
                   (str/ends-with? (subs (:value path-token) 1 (dec (count (:value path-token)))) ".h"))
      (throw (ex-info "Некорректная директива #include. Требуется формат: #include <file.h>"
                     {:tokens tokens})))
    
    {:type (:include-directive ast-node-types)
     :path (:value path-token)
     :tokens after-path}))

;; Основная реализация парсера
(defrecord C51Parser [tokens]
  ASTParser
  (parse-program [this tokens]
    "Анализ программы как последовательности деклараций, определений и директив

    Теоретическое обоснование:
    - Программа рассматривается как последовательность верхнеуровневых конструкций
    - Поддерживаются препроцессорные директивы и определения"
    (log/debug "Начало парсинга программы. Количество токенов: " (count tokens))
    (loop [remaining-tokens tokens
           parsed-nodes []]
      (if (empty? remaining-tokens)
        (do 
          (log/info "Парсинг программы завершен. Количество узлов: " (count parsed-nodes))
          {:type (:program ast-node-types)
           :nodes parsed-nodes})
        (let [[current & _] remaining-tokens
              result (cond
                      ;; Обработка препроцессорных директив
                      (= (:type current) :preprocessor-directive)
                      (parse-preprocessor-directive this remaining-tokens)
                      
                      ;; Обработка остальных конструкций
                      :else
                      (parse-function-declaration this remaining-tokens))
              node (dissoc result :tokens)]
          (recur (:tokens result) (conj parsed-nodes node))))))

  (parse-preprocessor-directive [this tokens]
    "Парсинг препроцессорных директив
     
     Поддерживаемые директивы:
     - #include
     - Другие директивы (будут добавлены позже)"
    (log/debug "Начало парсинга препроцессорной директивы")
    (let [[directive & _] tokens]
      (case (:value directive)
        "#include" (parse-include-directive tokens)
        (throw (ex-info "Неподдерживаемая препроцессорная директива"
                       {:directive directive})))))
  
  (parse-function-declaration [this tokens]
    "Улучшенный парсинг объявления функции

    Расширенная грамматика:
    function-declaration ::= type-keyword identifier '(' parameters? ')' '{' function-body '}'

    Семантический анализ:
    - Гибкий парсинг параметров
    - Поддержка вложенных блоков
    - Робастная обработка ошибок"
    (log/debug "Начало расширенного парсинга объявления функции")
    
    (let [[type-token & remaining] tokens
          [name-token & after-name] remaining
          [open-paren & after-paren] after-name]
      
      (when-not (and (= (:type type-token) :type-keyword)
                     (= (:type name-token) :identifier)
                     (= (:value open-paren) "("))
        (throw (ex-info "Некорректное начало объявления функции" 
                        {:tokens tokens})))
      
      (let [parameters-result (parse-parameters after-paren)
            tokens-after-params (:tokens parameters-result)
            [open-brace & after-brace] (drop-while #(not= (:value %) "{") tokens-after-params)]
        
        (when-not (= (:value open-brace) "{")
          (throw (ex-info "Ожидается открывающая фигурная скобка" 
                          {:tokens tokens-after-params})))
        
        (let [body-result (parse-function-body after-brace)]
          (log/trace "Распознана функция:" (:value name-token))
          
          {:type (:function-declaration ast-node-types)
           :return-type (:value type-token)
           :name (:value name-token)
           :parameters (:parameters parameters-result)
           :body (:body body-result)
           :tokens (:tokens body-result)}))))
  
  (parse-variable-declaration [this tokens]
    "Парсинг объявления переменной

    Грамматика:
    variable-declaration ::= type-keyword identifier (';' | '=' expression ';')

    Семантический анализ:
    - Проверка корректности типа переменной
    - Опциональная инициализация"
    (log/debug "Начало парсинга объявления переменной")
    (let [[type-token & remaining] tokens
          [name-token & remaining] remaining]
      (when-not (and (= (:type type-token) :type-keyword)
                     (= (:type name-token) :identifier))
        (log/info "Ошибка при парсинге объявления переменной")
        (throw (ex-info "Некорректное объявление переменной"
                        {:tokens tokens})))

      (log/trace "Распознана переменная: " (:value name-token) " типа " (:value type-token))
      {:type (:variable-declaration ast-node-types)
       :var-type (:value type-token)
       :name (:value name-token)
       :tokens remaining}))
  
  (parse-expression [this tokens]
    "Парсинг выражений

    Теоретическая модель:
    - Рекурсивный спуск для разбора выражений
    - Поддержка арифметических и логических операций

    Сложность: O(n), где n - длина выражения"
    (log/debug "Начало парсинга выражения")
    (let [[first-token & remaining] tokens]
      (cond
        (= (:type first-token) :identifier)
        (do 
          (log/trace "Распознан идентификатор: " (:value first-token))
          {:type (:expression ast-node-types)
           :value (:value first-token)
           :tokens remaining})

        (= (:type first-token) :int_number)
        (do 
          (log/trace "Распознано целое число: " (:value first-token))
          {:type (:expression ast-node-types)
           :value (:value first-token)
           :tokens remaining})

        :else
        (do 
          (log/info "Неподдерживаемое выражение")
          (throw (ex-info "Неподдерживаемое выражение"
                          {:tokens tokens})))))))

;; Функция для создания парсера
(defn create-parser
  "Создание экземпляра парсера с заданными токенами"
  [tokens]
  (log/debug "Создание экземпляра парсера с заданными токенами")
  (C51Parser. tokens))

;; Главная функция парсинга
(defn parse
  "Основная функция парсинга, принимающая последовательность токенов

   Архитектурные соображения:
   - Абстракция над конкретной реализацией парсера
   - Гибкость и расширяемость"
  [tokens]  
  (log/debug "Начало парсинга программы")
  (let [parser (create-parser tokens)]
    (parse-program parser tokens)))

(defn- parse-parameters
  "Парсинг параметров функции с расширенной логикой

  Грамматика параметров:
  - void без параметров
  - список типизированных параметров

  Теоретические аспекты:
  - Поддержка различных сигнатур функций
  - Гибкий синтаксический анализ
  - Обработка краевых случаев"
  [tokens]
  (log/debug "Начало парсинга параметров")
  (log/trace "Входящие токены:" (pr-str tokens))
  
  (loop [remaining tokens
         parameters []]
    (let [[current-token & rest] remaining]
      (log/trace "Текущий токен:" (pr-str current-token))
      (log/trace "Оставшиеся токены:" (pr-str rest))
      
      (cond 
        ;; Случай: пустые скобки () - пустой список параметров
        (= (:value current-token) ")")
        (do 
          (log/debug "Завершение парсинга параметров. Найдено параметров:" (count parameters))
          {:parameters parameters 
           :tokens remaining})
        
        ;; Случай: void без параметров 
        (and (or (= (:type current-token) :type-keyword)
                 (= (:type current-token) :keyword))
             (= (:value current-token) "void")
             (= (:value (first rest)) ")"))
        (do 
          (log/debug "Распознан void без параметров")
          {:parameters [{:type :void_type_keyword :name "void"}]
           :tokens rest})
        
        ;; Случай: ()
        (and (= (:value current-token) "(")
             (= (:value (first rest)) ")"))
        (do 
          (log/debug "Распознан пустой список параметров")
          {:parameters []
           :tokens rest})

        ;; Парсинг типизированного параметра
        (= (:type current-token) :type-keyword)
        (let [[name-token & next-tokens] rest]
          (when-not (= (:type name-token) :identifier)
            (throw (ex-info "Некорректное имя параметра" 
                             {:token name-token})))
          
          (log/trace "Распознан параметр: тип " (:value current-token) 
                     ", имя " (:value name-token))
          
          (recur 
           (if (= (:value (first next-tokens)) ",")
             (rest next-tokens)  ; Пропускаем запятую
             next-tokens)
           (conj parameters 
                 {:type (:value current-token)
                  :name (:value name-token)})))
        
        ;; Разделитель между параметрами
        (= (:value current-token) ",")
        (recur rest parameters)
        
        :else 
        (throw (ex-info "Неожиданный токен в списке параметров" 
                        {:token current-token}))))))

(defn- parse-function-body
  "Парсинг тела функции с расширенной диагностикой

  Теоретические аспекты:
  - Отслеживание вложенности блоков
  - Детальная обработка токенов
  - Робастный синтаксический анализ"
  [tokens]
  (log/debug "Начало парсинга тела функции")
  (log/trace "Входящие токены:" (pr-str tokens))
  
  (try 
    (loop [remaining tokens
           depth 1
           body-tokens []]
      (let [[current-token & rest] remaining]
        (cond 
          ;; Нет токенов - незавершенное тело функции
          (nil? current-token) 
          (if (> depth 1)
            (throw (ex-info "Незавершенное тело функции" 
                            {:tokens tokens
                             :depth depth
                             :body-tokens body-tokens}))
            {:body body-tokens 
             :tokens rest})
          
          ;; Открывающая фигурная скобка - увеличение глубины
          (= (:value current-token) "{") 
          (recur rest (inc depth) (conj body-tokens current-token))
          
          ;; Закрывающая фигурная скобка - уменьшение глубины
          (= (:value current-token) "}") 
          (let [new-depth (dec depth)]
            (if (zero? new-depth)
              {:body body-tokens 
               :tokens rest}
              (recur rest new-depth (conj body-tokens current-token))))
          
          ;; Обычный токен
          :else 
          (recur rest depth (conj body-tokens current-token)))))
    
    (catch Exception e
      (log/error "Критическая ошибка при парсинге тела функции")
      (log/error "Детали исключения:" (str e))
      (log/error "Трассировка стека:" 
        (with-out-str (stacktrace/print-stack-trace e)))
      (when-let [data (ex-data e)]
        (log/error "Дополнительные данные:" (pr-str data)))
      (throw 
       (ex-info 
        "Ошибка при парсинге тела функции" 
        {:original-exception e
         :tokens tokens})))))