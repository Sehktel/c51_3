(ns c51cc.parser
  "Модуль для синтаксического анализатора"
  (:require [c51cc.lexer  :as lexer]
            [c51cc.logger :as log]
            [clojure.stacktrace :as stacktrace]
            ))

;; Парсер для языка C51 - абстракция синтаксического анализа


;; Объявления функций для парсинга
(declare ast-node-types
        create-parser
        internal-parse-function-body
        internal-parse-parameters
        parse
        parse-array-declaration
        parse-array-initializer
        parse-assignment
        parse-binary-operation
        parse-bitwise-operation
        parse-break-continue
        parse-do-while
        parse-expression
        parse-expression-with-precedence
        parse-for-loop
        parse-function-call
        parse-function-declaration
        parse-if-else
        parse-pointer-declaration
        parse-program
        parse-return
        parse-struct-declaration
        parse-switch-case
        parse-typedef
        parse-unary-operation
        parse-variable-declaration
        parse-while-loop
        parse-with-context
        parse-data-space
        parse-xdata-space
        parse-code-space
        parse-default
        recognize-memory-types
        recognize-sfr-keywords
        recognize-bit-addressing
        validate-memory-constraints
        validate-interrupt-vectors
        validate-register-usage
        in-sfr-context?
        parse-bit-address)

(def ^:dynamic *current-debug-level* 
  "Динамическая переменная для текущего уровня логирования.
   По умолчанию установлен уровень INFO."
  :INFO)

;; Типы узлов абстрактного синтаксического дерева (AST)
(def ast-node-types
  "Типы узлов абстрактного синтаксического дерева (AST)"
  {:program :program
   :function-declaration :function-declaration
   :variable-declaration :variable-declaration
   :function-call :function-call
   :control-flow :control-flow
   :expression :expression})

(defn- ^:private internal-parse-parameters 
  "Семантический парсинг параметров функции"
  [tokens]
  (log/debug "Начало семантического парсинга параметров")
  
  (loop [remaining tokens
         parameters []]
    (let [[current-token & rest] remaining]
      (cond 
        ;; Случай: пустые скобки () - пустой список параметров
        (= (:value current-token) ")")
        {:parameters parameters 
         :tokens remaining}
        
        ;; Случай: void без параметров 
        (and (or (= (:type current-token) :type-keyword)
                 (= (:type current-token) :keyword))
             (= (:value current-token) "void")
             (= (:value (first rest)) ")"))
        {:parameters [{:type :void_type_keyword 
                       :name "void"}]
         :tokens rest}
        
        ;; Случай: ()
        (and (= (:value current-token) "(")
             (= (:value (first rest)) ")"))
        {:parameters []
         :tokens rest}

        ;; Парсинг типизированного параметра
        (= (:type current-token) :type-keyword)
        (let [[name-token & next-tokens] rest]
          (when-not (= (:type name-token) :identifier)
            (throw (ex-info "Некорректное имя параметра" 
                             {:token name-token})))
          
          (recur 
           (if (= (:value (first next-tokens)) ",")
             (rest next-tokens)  ; Пропускаем запятую
             next-tokens)
           (conj parameters 
                 {:type {:type :type-keyword
                         :value (:value current-token)}
                  :name {:type :identifier
                         :value (:value name-token)}})))
        
        ;; Разделитель между параметрами
        (= (:value current-token) ",")
        (recur rest parameters)
        
        :else 
        (throw (ex-info "Неожиданный токен в списке параметров" 
                        {:token current-token}))))))

(defn parse-function-declaration 
  "Улучшенный парсинг объявления функции

  Расширенная грамматика:
  function-declaration ::= type-keyword identifier '(' parameters? ')' interrupt? interrupt-number? '{' function-body '}'

  Семантический анализ:
  - Гибкий парсинг параметров
  - Поддержка вложенных блоков
  - Робастная обработка ошибок
  - Поддержка прерываний"
  [tokens]
  (log/debug "Начало расширенного парсинга объявления функции")
  (log/trace "Входящие токены для объявления функции: " (pr-str (take 5 tokens)))
  
  (let [[type-token & remaining] tokens
        [name-token & after-name] remaining
        [open-paren & after-paren] after-name]
    
    (when-not (and (= (:type type-token) :type-keyword)
                   (= (:type name-token) :identifier)
                   (= (:value open-paren) "("))
      (log/error "Некорректное начало объявления функции. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное начало объявления функции" 
                      {:tokens tokens})))
    
    (log/debug "Распознан тип функции: " (:value type-token) 
               ", имя функции: " (:value name-token))
    
    (let [parameters-result (internal-parse-parameters after-paren)
          tokens-after-params (:tokens parameters-result)
          
          ;; Проверка на ключевое слово interrupt и его номер
          interrupt-processing 
          (loop [tokens tokens-after-params
                 acc {:has-interrupt false}]
            (if (empty? tokens)
              acc
              (let [token (first tokens)]
                (cond 
                  (= (:value token) "interrupt")
                  (do 
                    (log/debug "Обнаружено ключевое слово interrupt")
                    (recur (rest tokens) (assoc acc :has-interrupt true)))
                  
                  (and (:has-interrupt acc)
                       (= (:type token) :int_number))
                  (do 
                    (log/debug "Распознан номер прерывания: " (:value token))
                    (recur (rest tokens) 
                           (assoc acc 
                                  :interrupt-number 
                                  (Integer/parseInt (:value token)))))
                  
                  (= (:value token) "{")
                  (reduced acc)
                  
                  :else
                  (recur (rest tokens) acc)))))
          
          ;; Поиск открывающей фигурной скобки
          [open-brace & after-brace] (drop-while #(not= (:value %) "{") 
                                                 tokens-after-params)]
      
      (when-not (= (:value open-brace) "{")
        (log/error "Ожидается открывающая фигурная скобка. Токены: " 
                   (pr-str tokens-after-params))
        (throw (ex-info "Ожидается открывающая фигурная скобка" 
                        {:tokens tokens-after-params})))
      
      (let [body-result (internal-parse-function-body after-brace)
            base-result {:type (:function-declaration ast-node-types)
                         :return-type {:type :type-keyword
                                       :value (:value type-token)}
                         :name {:type :identifier
                                :value (:value name-token)}
                         :parameters (:parameters parameters-result)
                         :body (:body body-result)
                         :tokens (:tokens body-result)}]
        
        (log/trace "Распознана функция:" (:value name-token) 
                   ", параметры: " (pr-str (:parameters parameters-result)))
        
        (if (:has-interrupt interrupt-processing)
          (let [result (assoc base-result 
                              :interrupt-number (:interrupt-number interrupt-processing))]
            (log/debug "Функция с прерыванием: " (:name result) 
                       ", номер прерывания: " (:interrupt-number result))
            result)
          base-result)))))

(defn- ^:private parse-variable-declaration 
  "Парсинг объявления переменной"
  [tokens]
  (log/debug "Начало парсинга объявления переменной")
  (log/trace "Входящие токены для объявления переменной: " (pr-str (take 5 tokens)))
  
  (let [[type-token & remaining] tokens
        [name-token & after-name] remaining]
    (when-not (and (= (:type type-token) :type-keyword)
                   (= (:type name-token) :identifier))
      (log/error "Ошибка при парсинге объявления переменной. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление переменной"
                      {:tokens tokens})))

    (log/info "Распознана переменная: " (:value name-token) 
              " типа " (:value type-token))
    
    {:type (:variable-declaration ast-node-types)
     :var-type (str (:value type-token))
     :name (:value name-token)
     :tokens after-name}))

(defn- ^:private parse-expression
  "Парсинг выражений с поддержкой сложных конструкций
   
   Ключевые возможности:
   - Поддержка идентификаторов и чисел
   - Обработка операторов
   - Поддержка разделителей
   - Обработка пустых выражений"
  [tokens]
  (log/debug "Начало парсинга выражения")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))
  
  (let [[first-token & remaining] tokens]
    (cond
      ;; Пустое выражение или разделитель
      (or (nil? first-token)
          (= ")" (:value first-token)))
      {:type (:expression ast-node-types)
       :value ""
       :tokens tokens}

      ;; Идентификатор
      (= (:type first-token) :identifier)
      {:type (:expression ast-node-types)
       :value (:value first-token)
       :tokens remaining}

      ;; Число
      (= (:type first-token) :int_number)
      {:type (:expression ast-node-types)
       :value (:value first-token)
       :tokens remaining}

      ;; Оператор
      (= (:type first-token) :operator)
      (let [left-expr (parse-expression remaining)
            [operator & after-op] (:tokens left-expr)
            right-expr (parse-expression after-op)]
        {:type (:expression ast-node-types)
         :value (str (:value left-expr) 
                     (:value operator) 
                     (:value right-expr))
         :tokens (:tokens right-expr)})
      
      ;; Разделители и другие токены
      (= (:type first-token) :separator)
      {:type (:expression ast-node-types)
       :value (:value first-token)
       :tokens remaining}
      
      :else
      (throw (ex-info "Неподдерживаемое выражение"
                      {:tokens tokens})))))

(defn- ^:private parse-assignment 
  "Семантический парсинг присваивания"
  [tokens]
  (log/debug "Начало парсинга присваивания")
  
  (let [[left-token & after-left] tokens
        [op-token & after-op] after-left
        [right-token & after-right] after-op]
    (when-not (and (= (:type left-token) :identifier)
                   (= (:value op-token) "="))
      (throw (ex-info "Некорректное присваивание"
                      {:tokens tokens})))
    
    {:type (:assignment ast-node-types)
     :left {:type (:identifier ast-node-types)
            :value (:value left-token)}
     :right (parse-expression (list right-token))
     :tokens after-right}))

(defn- ^:private internal-parse-function-body 
  "Парсинг тела функции с расширенной диагностикой"
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
          
          ;; Объявление переменной
          (= (:type current-token) :type-keyword)
          (let [var-decl (parse-variable-declaration (cons current-token rest))]
            (recur (:tokens var-decl) depth 
                   (conj body-tokens var-decl)))
          
          ;; Выражение или присваивание
          (or (= (:type current-token) :identifier)
              (= (:type current-token) :int_number))
          (let [expr (parse-expression (cons current-token rest))]
            (recur (:tokens expr) depth 
                   (conj body-tokens expr)))
          
          ;; Пропускаем разделители
          (= (:value current-token) ";")
          (recur rest depth body-tokens)
          
          ;; Цикл for
          (= (:value current-token) "for")
          (let [for-loop (parse-for-loop (cons current-token rest))]
            (recur (:tokens for-loop) depth 
                   (conj body-tokens for-loop)))
          
          ;; Цикл while
          (= (:value current-token) "while")
          (let [while-loop (parse-while-loop (cons current-token rest))]
            (recur (:tokens while-loop) depth 
                   (conj body-tokens while-loop)))
          
          ;; Вызов функции
          (and (= (:type current-token) :identifier)
               (= (:value (first rest)) "("))
          (let [func-call (parse-function-call (cons current-token rest))]
            (recur (:tokens func-call) depth 
                   (conj body-tokens func-call)))
          
          ;; Конструкция switch-case
          (= (:value current-token) "switch")
          (let [switch-case (parse-switch-case (cons current-token rest))]
            (recur (:tokens switch-case) depth 
                   (conj body-tokens switch-case)))
          
          ;; Цикл do-while
          (= (:value current-token) "do")
          (let [do-while-loop (parse-do-while (cons current-token rest))]
            (recur (:tokens do-while-loop) depth 
                   (conj body-tokens do-while-loop)))
          
          ;; Конструкция if-else
          (= (:value current-token) "if")
          (let [if-else-block (parse-if-else (cons current-token rest))]
            (recur (:tokens if-else-block) depth 
                   (conj body-tokens if-else-block)))
          
          ;; Оператор break
          (= (:value current-token) "break")
          (let [break-stmt (parse-break-continue (cons current-token rest))]
            (recur (:tokens break-stmt) depth 
                   (conj body-tokens break-stmt)))
          
          ;; Оператор continue
          (= (:value current-token) "continue")
          (let [continue-stmt (parse-break-continue (cons current-token rest))]
            (recur (:tokens continue-stmt) depth 
                   (conj body-tokens continue-stmt)))
          
          ;; Оператор return
          (= (:value current-token) "return")
          (let [return-stmt (parse-return (cons current-token rest))]
            (recur (:tokens return-stmt) depth 
                   (conj body-tokens return-stmt)))
          
          ;; Объявление указателя
          (and (= (:type current-token) :type-keyword)
               (= (:value (first rest)) "*"))
          (let [pointer-decl (parse-pointer-declaration (cons current-token rest))]
            (recur (:tokens pointer-decl) depth 
                   (conj body-tokens pointer-decl)))
          
          ;; Объявление массива
          (and (= (:type current-token) :type-keyword)
               (= (:type (first rest)) :identifier)
               (= (:value (second rest)) "["))
          (let [array-decl (parse-array-declaration (cons current-token rest))]
            (recur (:tokens array-decl) depth 
                   (conj body-tokens array-decl)))
          
          ;; Объявление структуры
          (= (:value current-token) "struct")
          (let [struct-decl (parse-struct-declaration (cons current-token rest))]
            (recur (:tokens struct-decl) depth 
                   (conj body-tokens struct-decl)))
          
          ;; Объявление typedef
          (= (:value current-token) "typedef")
          (let [typedef-decl (parse-typedef (cons current-token rest))]
            (recur (:tokens typedef-decl) depth 
                   (conj body-tokens typedef-decl)))
          
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

;; Реализации парсера
(defn parse-program
  "Семантический анализ программы с созданием полноценного AST"
  ([tokens]
   (parse-program tokens nil))
  ([tokens context]
   (log/debug "Начало семантического парсинга программы")
   (loop [remaining-tokens tokens
          parsed-nodes []]
     (if (empty? remaining-tokens)
       {:type (:program ast-node-types)
        :nodes parsed-nodes}
       (let [result (parse-function-declaration remaining-tokens)
             node (dissoc result :tokens)]
         (recur (:tokens result) (conj parsed-nodes node)))))))

;; Добавление парсера для циклов for
(defn parse-for-loop 
  "Семантический парсинг цикла for с расширенной диагностикой

  Грамматика цикла for:
  for (initialization; condition; increment) {
    body
  }

  Ключевые аспекты парсинга:
  - Строгий синтаксический контроль
  - Поддержка сложных инициализаций, условий и инкрементов
  - Робастная обработка ошибок"
  [tokens]
  (log/debug "Начало парсинга цикла for")
  (log/trace "Входящие токены для цикла for: " (pr-str (take 10 tokens)))

  (let [[for-token & after-for] tokens
        [open-paren & after-open] after-for]
    
    (when-not (and (= (:value for-token) "for")
                   (= (:value open-paren) "("))
      (log/error "Некорректное начало цикла for. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление цикла for" 
                      {:tokens tokens})))
    
    (log/debug "Распознано ключевое слово for")
    
    ;; Парсинг инициализации
    (let [init-result (parse-variable-declaration (rest after-open))
          [semicolon1 & after-init] (:tokens init-result)
          
          ;; Парсинг условия
          condition-result (parse-expression after-init)
          [semicolon2 & after-condition] (:tokens condition-result)
          
          ;; Парсинг инкремента
          increment-result (parse-expression (rest after-condition))
          [close-paren & after-close] (:tokens increment-result)
          
          ;; Парсинг тела цикла
          [open-brace & after-open-brace] after-close
          body-result (internal-parse-function-body after-open-brace)]
      
      (when-not (and (= (:value semicolon1) ";")
                     (= (:value semicolon2) ";")
                     (= (:value close-paren) ")")
                     (= (:value open-brace) "{"))
        (log/error "Некорректный синтаксис цикла for")
        (throw (ex-info "Ошибка в синтаксисе цикла for"
                        {:tokens tokens})))
      
      (log/info "Успешный парсинг цикла for")
      
      {:type (:control-flow ast-node-types)
       :subtype :for-loop
       :initialization init-result
       :condition condition-result
       :increment increment-result
       :body (:body body-result)
       :tokens (:tokens body-result)})))

;; Объявление парсера для while-циклов
(declare parse-while-loop)

;; Парсер для while-циклов
(defn parse-while-loop
  "Семантический парсинг цикла while с расширенной диагностикой

  Грамматика цикла while:
  while (condition) {
    body
  }

  Ключевые аспекты парсинга:
  - Строгий синтаксический контроль
  - Поддержка сложных условий
  - Робастная обработка ошибок"
  [tokens]
  (log/debug "Начало парсинга цикла while")
  (log/trace "Входящие токены для цикла while: " (pr-str (take 10 tokens)))

  (let [[while-token & after-while] tokens
        [open-paren & after-open] after-while]
    
    (when-not (and (= (:value while-token) "while")
                   (= (:value open-paren) "("))
      (log/error "Некорректное начало цикла while. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление цикла while" 
                      {:tokens tokens})))
    
    (log/debug "Распознано ключевое слово while")
    
    ;; Парсинг условия
    (let [condition-result (parse-expression (rest after-open))
          [close-paren & after-condition] (:tokens condition-result)
          
          ;; Парсинг тела цикла
          [open-brace & after-open-brace] after-condition
          body-result (internal-parse-function-body after-open-brace)]
      
      (when-not (and (= (:value close-paren) ")")
                     (= (:value open-brace) "{"))
        (log/error "Некорректный синтаксис цикла while")
        (throw (ex-info "Ошибка в синтаксисе цикла while"
                        {:tokens tokens})))
      
      (log/info "Успешный парсинг цикла while")
      
      {:type (:control-flow ast-node-types)
       :subtype :while-loop
       :condition condition-result
       :body (:body body-result)
       :tokens (:tokens body-result)})))

;; Объявление парсера вызова функций
(declare parse-function-call)

(defn- ^:private parse-function-arguments 
  "Семантический парсинг аргументов функции

  Ключевые возможности:
  - Поддержка различных типов аргументов
  - Рекурсивный парсинг сложных выражений
  - Обработка пустого списка аргументов"
  [tokens]
  (log/debug "Начало парсинга аргументов функции")
  (log/trace "Входящие токены аргументов: " (pr-str (take 10 tokens)))

  (loop [remaining tokens
         arguments []]
    (let [[current-token & rest] remaining]
      (cond
        ;; Закрывающая скобка - конец списка аргументов
        (= (:value current-token) ")")
        {:arguments arguments 
         :tokens remaining}
        
        ;; Пустые аргументы
        (and (= (:value current-token) "(")
             (= (:value (first rest)) ")"))
        {:arguments []
         :tokens rest}
        
        ;; Разделитель аргументов
        (= (:value current-token) ",")
        (recur rest arguments)
        
        ;; Парсинг аргумента как выражения
        :else
        (let [arg-result (parse-expression (cons current-token rest))]
          (recur (:tokens arg-result) 
                 (conj arguments arg-result)))))))

(defn- ^:private parse-function-call 
  "Семантический парсинг вызова функции с аргументами

  Грамматика вызова функции:
  function_name(arg1, arg2, ...)

  Ключевые аспекты парсинга:
  - Распознавание имени функции
  - Парсинг списка аргументов
  - Поддержка сложных выражений в аргументах"
  [tokens]
  (log/debug "Начало парсинга вызова функции")
  (log/trace "Входящие токены вызова функции: " (pr-str (take 10 tokens)))

  (let [[func-name-token & after-name] tokens
        [open-paren & after-open] after-name]
    
    (when-not (and (= (:type func-name-token) :identifier)
                   (= (:value open-paren) "("))
      (log/error "Некорректный вызов функции. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректный вызов функции" 
                      {:tokens tokens})))
    
    (log/debug "Распознано имя функции: " (:value func-name-token))
    
    ;; Парсинг аргументов
    (let [args-result (parse-function-arguments (rest after-open))
          [close-paren & after-close] (:tokens args-result)]
      
      (when-not (= (:value close-paren) ")")
        (log/error "Ожидается закрывающая скобка в вызове функции")
        (throw (ex-info "Ошибка в синтаксисе вызова функции"
                        {:tokens tokens})))
      
      (log/info "Успешный парсинг вызова функции: " (:value func-name-token))
      
      {:type (:function-call ast-node-types)
       :function-name {:type :identifier
                       :value (:value func-name-token)}
       :arguments (:arguments args-result)
       :tokens after-close})))

;; Объявление парсера switch-case
(declare parse-switch-case)

(defn parse-switch-case
  "Семантический парсинг конструкции switch-case с расширенной диагностикой

  Грамматика switch-case:
  switch (expression) {
    case constant1:
      statements1
      break;
    case constant2:
      statements2
      break;
    default:
      default_statements
  }

  Ключевые аспекты парсинга:
  - Распознавание выражения switch
  - Парсинг множественных case-блоков
  - Поддержка default-блока
  - Робастная обработка вложенных блоков"
  [tokens]
  (log/debug "Начало парсинга конструкции switch-case")
  (log/trace "Входящие токены switch-case: " (pr-str (take 10 tokens)))

  (let [[switch-token & after-switch] tokens
        [open-paren & after-open] after-switch]
    
    (when-not (and (= (:value switch-token) "switch")
                   (= (:value open-paren) "("))
      (log/error "Некорректное начало конструкции switch-case. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление switch-case" 
                      {:tokens tokens})))
    
    (log/debug "Распознано ключевое слово switch")
    
    ;; Парсинг выражения switch
    (let [[expr-token & remaining] after-open
          switch-expr {:type (:expression ast-node-types)
                      :value (:value expr-token)}
          [close-paren & after-condition] remaining
          
          ;; Парсинг тела switch
          [open-brace & after-open-brace] after-condition]
      
      (when-not (and (= (:value close-paren) ")")
                     (= (:value open-brace) "{"))
        (log/error "Некорректный синтаксис switch-case")
        (throw (ex-info "Ошибка в синтаксисе switch-case"
                        {:tokens tokens})))
      
      ;; Парсинг case-блоков и default
      (loop [remaining (rest after-open-brace)
             depth 1
             cases []
             default-case nil]
        (let [[current-token & rest] remaining]
          (cond
            ;; Конец switch-блока
            (and (= (:value current-token) "}")
                 (= depth 1))
            {:type (:control-flow ast-node-types)
             :subtype :switch-case
             :switch-expression switch-expr
             :cases cases
             :default-case default-case
             :tokens (cons current-token rest)}
            
            ;; Открытие вложенного блока
            (= (:value current-token) "{")
            (recur rest (inc depth) cases default-case)
            
            ;; Закрытие вложенного блока
            (= (:value current-token) "}")
            (recur rest (dec depth) cases default-case)
            
            ;; Парсинг case-блока
            (= (:value current-token) "case")
            (let [[const-token & after-const] rest
                  case-const {:type (:expression ast-node-types)
                             :value (:value const-token)}
                  [colon & after-colon] after-const
                  
                  ;; Парсинг тела case
                  case-body (internal-parse-function-body after-colon)]
              (when-not (= (:value colon) ":")
                (log/error "Ожидается ':' после константы case")
                (throw (ex-info "Некорректный синтаксис case"
                                {:tokens tokens})))
              
              (recur (:tokens case-body) 
                     depth 
                     (conj cases 
                           {:type :case-block
                            :constant case-const
                            :body (:body case-body)})
                     default-case))
            
            ;; Парсинг default-блока
            (= (:value current-token) "default")
            (let [[colon & after-colon] rest
                  
                  ;; Парсинг тела default
                  default-body (internal-parse-function-body after-colon)]
              (when-not (= (:value colon) ":")
                (log/error "Ожидается ':' после default")
                (throw (ex-info "Некорректный синтаксис default"
                                {:tokens tokens})))
              
              (when default-case
                (log/error "Множественные default-блоки не допускаются")
                (throw (ex-info "Множественные default-блоки"
                                {:tokens tokens})))
              
              (recur (:tokens default-body) 
                     depth 
                     cases 
                     {:type :default-block
                      :body (:body default-body)}))
            
            ;; Пропуск других токенов
            :else
            (recur rest depth cases default-case)))))))

;; Определение приоритетов операторов
(def ^:private operator-precedence
  "Таблица приоритетов бинарных операторов

  Уровни приоритета:
  1 - низший приоритет (логические операции)
  2 - сравнение
  3 - сложение/вычитание
  4 - умножение/деление
  5 - высший приоритет (скобки, унарные операции)"
  {:|| 1
   :&& 1
   :|  1
   :&  1
   :== 2
   :!= 2
   :<  2
   :>  2
   :<= 2
   :>= 2
   :+  3
   :-  3
   :*  4
   :/  4
   :%  4})

;; Множество поддерживаемых бинарных операторов
(def ^:private binary-operators
  "Множество поддерживаемых бинарных операторов"
  #{:+ :- :* :/ :% 
    :== :!= :< :> :<= :>= 
    :&& :|| :& :|})

(defn- ^:private is-binary-operator?
  "Проверка, является ли токен бинарным оператором"
  [token]
  (contains? binary-operators (keyword (:value token))))

(defn- ^:private parse-binary-operation 
  "Семантический парсинг бинарных операций с учетом приоритета операторов

  Ключевые возможности:
  - Поддержка сложных выражений
  - Учет приоритета операторов
  - Рекурсивный парсинг подвыражений"
  [tokens]
  (log/debug "Начало парсинга бинарной операции")
  (log/trace "Входящие токены: " (pr-str (take 10 tokens)))
  
  (let [parse-expression-with-precedence 
        (fn parse-expr-precedence [tokens precedence]
          (let [[first-token & rest] tokens]
            (if (nil? first-token)
              (throw (ex-info "Неожиданный конец выражения" 
                              {:tokens tokens}))
              
              ;; Парсинг первичного выражения (операнда)
              (let [left-expr 
                    (cond
                      ;; Идентификатор или число
                      (or (= (:type first-token) :identifier)
                          (= (:type first-token) :int_number))
                      {:type (:expression ast-node-types)
                       :value (:value first-token)
                       :tokens rest}
                      
                      ;; Скобки - группировка выражения
                      (= (:value first-token) "(")
                      (let [grouped-expr (parse-expr-precedence rest 0)]
                        (when-not (= (:value (first (:tokens grouped-expr))) ")")
                          (throw (ex-info "Ожидается закрывающая скобка" 
                                          {:tokens tokens})))
                        (assoc grouped-expr 
                               :tokens (rest (:tokens grouped-expr))))
                      
                      :else 
                      (throw (ex-info "Некорректное начало выражения" 
                                      {:tokens tokens})))]
                
                ;; Проверка наличия бинарного оператора
                (loop [current-expr left-expr
                       remaining (:tokens current-expr)]
                  (let [[op-token & after-op] remaining]
                    (cond
                      ;; Нет токенов или не бинарный оператор
                      (or (nil? op-token)
                          (not (is-binary-operator? op-token)))
                      (assoc current-expr :tokens remaining)
                      
                      ;; Проверка приоритета оператора
                      (< (get operator-precedence (keyword (:value op-token)) 0) 
                         precedence)
                      (assoc current-expr :tokens remaining)
                      
                      ;; Парсинг правого операнда
                      :else
                      (let [right-expr (parse-expr-precedence 
                                        after-op 
                                        (inc (get operator-precedence 
                                                 (keyword (:value op-token)) 
                                                 0)))]
                        (recur 
                         {:type (:expression ast-node-types)
                          :value (str (:value current-expr) 
                                      (:value op-token) 
                                      (:value right-expr))
                          :tokens (:tokens right-expr)}
                         (:tokens right-expr))))))))))
        
        parse-expression 
        (fn [tokens]
          (parse-expression-with-precedence tokens 0))]
    
    ;; Вызов парсера выражения
    (parse-expression tokens)))

;; Множество унарных операторов
(def ^:private unary-operators
  "Множество поддерживаемых унарных операторов"
  #{"~" "!"})

;; Множество битовых операторов
(def ^:private bitwise-operators
  "Множество поддерживаемых битовых операторов"
  #{"<<" ">>" "&" "|" "^"})

;; Расширенная таблица приоритетов с унарными операциями
(def ^:private extended-operator-precedence
  "Расширенная таблица приоритетов операторов с учетом унарных операций"
  {"||" 1
   "&&" 1
   "|"  1
   "&"  1
   "==" 2
   "!=" 2
   "<"  2
   ">"  2
   "<=" 2
   ">=" 2
   "+"  3
   "-"  3
   "*"  4
   "/"  4
   "%"  4
   "++" 5
   "--" 5
   "!"  5
   "~"  5
   "<<" 4
   ">>" 4
   "^"  2})

(defn- ^:private is-unary-operator?
  "Проверка, является ли токен унарным оператором"
  [token]
  (contains? unary-operators (keyword (:value token))))

(defn- ^:private is-bitwise-operator? 
  "Проверка, является ли токен битовым оператором"
  [token]
  (contains? bitwise-operators (keyword (:value token))))

(defn- ^:private parse-unary-operation 
  "Семантический парсинг унарных операций

  Ключевые возможности:
  - Префиксные и постфиксные унарные операции
  - Поддержка инкремента/декремента
  - Логические и битовые унарные операции"
  [tokens]
  (log/debug "Начало парсинга унарной операции")
  (log/trace "Входящие токены: " (pr-str (take 10 tokens)))
  
  (let [[first-token & rest] tokens]
    (cond
      ;; Префиксные унарные операторы
      (is-unary-operator? first-token)
      (let [operand (parse-binary-operation rest)]
        {:type (:expression ast-node-types)
         :value (str (:value first-token) (:value operand))
         :tokens (:tokens operand)})
      
      ;; Постфиксные унарные операторы (++ и --)
      (and (= (:type first-token) :identifier)
           (is-unary-operator? (first rest)))
      (let [[op-token & after-op] rest]
        {:type (:expression ast-node-types)
         :value (str (:value first-token) (:value op-token))
         :tokens after-op})
      
      ;; Стандартное выражение
      :else
      (parse-binary-operation tokens))))

(defn- ^:private parse-bitwise-operation 
  "Семантический парсинг битовых операций

  Ключевые возможности:
  - Поддержка битовых операторов
  - Учет приоритета операций
  - Рекурсивный парсинг подвыражений"
  [tokens]
  (log/debug "Начало парсинга битовой операции")
  (log/trace "Входящие токены: " (pr-str (take 10 tokens)))
  
  (let [parse-expression-with-precedence 
        (fn [tokens precedence]
          (let [[first-token & rest] tokens]
            (if (nil? first-token)
              (throw (ex-info "Неожиданный конец выражения" 
                               {:tokens tokens}))
              
              ;; Парсинг первичного выражения (операнда)
              (let [left-expr 
                    (cond
                      ;; Идентификатор или число
                      (or (= (:type first-token) :identifier)
                          (= (:type first-token) :int_number))
                      {:type (:expression ast-node-types)
                       :value (:value first-token)
                       :tokens rest}
                      
                      ;; Скобки - группировка выражения
                      (= (:value first-token) "(")
                      (let [grouped-expr (parse-unary-operation rest)]
                        (when-not (= (:value (first (:tokens grouped-expr))) ")")
                          (throw (ex-info "Ожидается закрывающая скобка" 
                                          {:tokens tokens})))
                        (assoc grouped-expr 
                               :tokens (rest (:tokens grouped-expr))))
                      
                      ;; Унарная операция
                      (is-unary-operator? first-token)
                      (parse-unary-operation tokens)
                      
                      :else 
                      (throw (ex-info "Некорректное начало выражения" 
                                      {:tokens tokens})))]
                
                ;; Проверка наличия битового оператора
                (loop [current-expr left-expr
                       remaining (:tokens current-expr)]
                  (let [[op-token & after-op] remaining]
                    (cond
                      ;; Нет токенов или не битовый оператор
                      (or (nil? op-token)
                          (not (is-bitwise-operator? op-token)))
                      (assoc current-expr :tokens remaining)
                      
                      ;; Проверка приоритета оператора
                      (< (get extended-operator-precedence 
                               (keyword (:value op-token)) 0) 
                         precedence)
                      (assoc current-expr :tokens remaining)
                      
                      ;; Парсинг правого операнда
                      :else
                      (let [right-expr 
                            (parse-expression-with-precedence 
                             after-op 
                             (inc (get extended-operator-precedence 
                                       (keyword (:value op-token)) 
                                       0)))]
                        (recur 
                         {:type (:expression ast-node-types)
                          :value (str (:value current-expr) 
                                      (:value op-token) 
                                      (:value right-expr))
                          :tokens (:tokens right-expr)}
                         (:tokens right-expr))))))))))
        
        parse-expression 
        (fn [tokens]
          (parse-expression-with-precedence tokens 0))]
    
    ;; Вызов парсера выражения
    (parse-expression tokens)))

;; Парсер для if-else
(defn parse-if-else 
  "Семантический парсинг конструкции if-else

  Грамматика if-else:
  if (condition) {
    true_block
  } else {
    false_block
  }

  Ключевые аспекты парсинга:
  - Распознавание условия
  - Парсинг блоков true и false
  - Поддержка вложенных блоков"
  [tokens]
  (log/debug "Начало парсинга конструкции if-else")
  (log/trace "Входящие токены if-else: " (pr-str (take 10 tokens)))

  (let [[if-token & after-if] tokens
        [open-paren & after-open] after-if]
    
    (when-not (and (= (:value if-token) "if")
                   (= (:value open-paren) "("))
      (log/error "Некорректное начало конструкции if-else. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление if-else" 
                      {:tokens tokens})))
    
    (log/debug "Распознано ключевое слово if")
    
    ;; Парсинг условия
    (let [condition-expr (parse-expression (rest after-open))
          [close-paren & after-condition] (:tokens condition-expr)
          
          ;; Парсинг блока true
          [true-open-brace & after-true-open] after-condition
          true-body (internal-parse-function-body after-true-open)]
      
      (when-not (= (:value close-paren) ")")
        (log/error "Некорректный синтаксис if")
        (throw (ex-info "Ошибка в синтаксисе if"
                        {:tokens tokens})))
      
      ;; Проверка наличия else
      (let [[else-or-next-token & rest-tokens] (:tokens true-body)]
        (if (and else-or-next-token 
                 (= (:value else-or-next-token) "else"))
          (let [[else-open-brace & after-else-open] rest-tokens
                false-body (internal-parse-function-body after-else-open)]
            {:type (:control-flow ast-node-types)
             :subtype :if-else
             :condition condition-expr
             :true-block (:body true-body)
             :false-block (:body false-body)
             :tokens (:tokens false-body)})
          
          ;; Простой if без else
          {:type (:control-flow ast-node-types)
           :subtype :if
           :condition condition-expr
           :true-block (:body true-body)
           :tokens (:tokens true-body)})))))

;; Парсер для do-while
(defn parse-do-while 
  "Семантический парсинг цикла do-while

  Грамматика do-while:
  do {
    body
  } while (condition);

  Ключевые аспекты парсинга:
  - Парсинг тела цикла
  - Распознавание условия
  - Поддержка сложных блоков"
  [tokens]
  (log/debug "Начало парсинга цикла do-while")
  (log/trace "Входящие токены do-while: " (pr-str (take 10 tokens)))

  (let [[do-token & after-do] tokens
        [open-brace & after-open] after-do]
    
    (when-not (and (= (:value do-token) "do")
                   (= (:value open-brace) "{"))
      (log/error "Некорректное начало конструкции do-while. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление do-while" 
                      {:tokens tokens})))
    
    (log/debug "Распознано ключевое слово do")
    
    ;; Парсинг тела цикла
    (let [body-result (internal-parse-function-body after-open)
          [while-token & after-while] (:tokens body-result)
          [open-paren & after-open] after-while]
      
      (when-not (and (= (:value while-token) "while")
                     (= (:value open-paren) "("))
        (log/error "Ожидается while после тела цикла")
        (throw (ex-info "Некорректный синтаксис do-while"
                        {:tokens tokens})))
      
      ;; Парсинг условия
      (let [condition-expr (parse-expression (rest after-open))
            [close-paren & after-condition] (:tokens condition-expr)
            [semicolon & after-semicolon] after-condition]
        
        (when-not (and (= (:value close-paren) ")")
                       (= (:value semicolon) ";"))
          (log/error "Некорректный синтаксис do-while")
          (throw (ex-info "Ошибка в синтаксисе do-while"
                          {:tokens tokens})))
        
        {:type (:control-flow ast-node-types)
         :subtype :do-while
         :body (:body body-result)
         :condition condition-expr
         :tokens after-semicolon}))))

;; Парсер для break и continue
(defn parse-break-continue
  "Семантический парсинг операторов break и continue

  Грамматика:
  break;
  continue;

  Ключевые аспекты парсинга:
  - Распознавание ключевых слов
  - Проверка наличия точки с запятой"
  [tokens]
  (log/debug "Начало парсинга break/continue")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[first-token & rest] tokens]
    (when-not (or (= (:value first-token) "break")
                  (= (:value first-token) "continue"))
      (log/error "Некорректный оператор break/continue. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректный оператор break/continue" 
                      {:tokens tokens})))
    
    (let [[semicolon & after-semicolon] rest]
      (when-not (= (:value semicolon) ";")
        (log/error "Ожидается точка с запятой после break/continue")
        (throw (ex-info "Отсутствует точка с запятой"
                        {:tokens tokens})))
      
      {:type (:control-flow ast-node-types)
       :subtype (keyword (:value first-token))
       :tokens after-semicolon})))

;; Парсер для return
(defn parse-return 
  "Семантический парсинг оператора return

  Грамматика:
  return expression?;

  Ключевые аспекты парсинга:
  - Распознавание ключевого слова return
  - Опциональный возврат выражения
  - Проверка точки с запятой"
  [tokens]
  (log/debug "Начало парсинга return")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[return-token & rest] tokens]
    (when-not (= (:value return-token) "return")
      (log/error "Некорректный оператор return. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректный оператор return" 
                      {:tokens tokens})))
    
    (let [[next-token & after-next] rest]
      (if (= (:value next-token) ";")
        ;; Пустой return
        {:type (:control-flow ast-node-types)
         :subtype :return
         :value nil
         :tokens after-next}
        
        ;; Return с выражением
        (let [return-expr (parse-expression (cons next-token rest))
              [semicolon & after-semicolon] (:tokens return-expr)]
          (when-not (= (:value semicolon) ";")
            (log/error "Ожидается точка с запятой после return")
            (throw (ex-info "Отсутствует точка с запятой"
                            {:tokens tokens})))
          
          {:type (:control-flow ast-node-types)
           :subtype :return
           :value return-expr
           :tokens after-semicolon})))))

;; Парсер для указателей
(defn parse-pointer-declaration 
  "Семантический парсинг объявления указателей

  Грамматика:
  type* identifier;
  type* identifier = expression;

  Ключевые аспекты парсинга:
  - Распознавание типа указателя
  - Поддержка инициализации
  - Проверка синтаксиса"
  [tokens]
  (log/debug "Начало парсинга объявления указателя")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[type-token & rest] tokens]
    (when-not (= (:type type-token) :type-keyword)
      (log/error "Некорректный тип указателя. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректный тип указателя" 
                      {:tokens tokens})))
    
    (let [[asterisk & after-asterisk] rest]
      (when-not (= (:value asterisk) "*")
        (log/error "Ожидается '*' для объявления указателя")
        (throw (ex-info "Отсутствует '*' для указателя"
                        {:tokens tokens})))
      
      (let [[name-token & after-name] after-asterisk]
        (when-not (= (:type name-token) :identifier)
          (log/error "Некорректное имя указателя")
          (throw (ex-info "Некорректное имя указателя"
                          {:tokens tokens})))
        
        (let [[next-token & after-next] after-name]
          (cond
            ;; Простое объявление указателя
            (= (:value next-token) ";")
            {:type (:variable-declaration ast-node-types)
             :var-type (str (:value type-token) "*")
             :name (:value name-token)
             :is-pointer true
             :tokens after-next}
            
            ;; Инициализация указателя
            (= (:value next-token) "=")
            (let [init-expr (parse-expression (rest after-name))
                  [semicolon & after-semicolon] (:tokens init-expr)]
              (when-not (= (:value semicolon) ";")
                (log/error "Ожидается точка с запятой после инициализации указателя")
                (throw (ex-info "Отсутствует точка с запятой"
                                {:tokens tokens})))
              
              {:type (:variable-declaration ast-node-types)
               :var-type (str (:value type-token) "*")
               :name (:value name-token)
               :is-pointer true
               :init-value init-expr
               :tokens after-semicolon})
            
            :else
            (throw (ex-info "Некорректный синтаксис объявления указателя"
                            {:tokens tokens}))))))))

;; Парсер для массивов
(defn parse-array-declaration 
  "Семантический парсинг объявления массивов

  Грамматика:
  type identifier[size];
  type identifier[size] = {init_values};

  Ключевые аспекты парсинга:
  - Распознавание типа массива
  - Поддержка размера и инициализации
  - Проверка синтаксиса"
  [tokens]
  (log/debug "Начало парсинга объявления массива")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[type-token & rest] tokens]
    (when-not (= (:type type-token) :type-keyword)
      (log/error "Некорректный тип массива. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректный тип массива" 
                      {:tokens tokens})))
    
    (let [[name-token & after-name] rest]
      (when-not (= (:type name-token) :identifier)
        (log/error "Некорректное имя массива")
        (throw (ex-info "Некорректное имя массива"
                        {:tokens tokens})))
      
      (let [[open-bracket & after-open] after-name]
        (when-not (= (:value open-bracket) "[")
          (log/error "Ожидается '[' для объявления массива")
          (throw (ex-info "Отсутствует '[' для массива"
                          {:tokens tokens})))
        
        (let [size-expr (parse-expression (rest after-open))
              [close-bracket & after-close] (:tokens size-expr)]
          (when-not (= (:value close-bracket) "]")
            (log/error "Ожидается ']' после размера массива")
            (throw (ex-info "Отсутствует ']'"
                            {:tokens tokens})))
          
          (let [[next-token & after-next] after-close]
            (cond
              ;; Простое объявление массива
              (= (:value next-token) ";")
              {:type (:variable-declaration ast-node-types)
               :var-type (str (:value type-token) "[]")
               :name (:value name-token)
               :is-array true
               :array-size size-expr
               :tokens after-next}
              
              ;; Инициализация массива
              (= (:value next-token) "=")
              (let [[open-brace & after-open] (rest after-next)
                    init-values (parse-array-initializer 
                                 (cons open-brace after-open))]
                (when-not (= (:value open-brace) "{")
                  (log/error "Ожидается '{' для инициализации массива")
                  (throw (ex-info "Отсутствует '{' для инициализации"
                                  {:tokens tokens})))
                
                {:type (:variable-declaration ast-node-types)
                 :var-type (str (:value type-token) "[]")
                 :name (:value name-token)
                 :is-array true
                 :array-size size-expr
                 :init-values init-values
                 :tokens (:tokens init-values)})
              
              :else
              (throw (ex-info "Некорректный синтаксис объявления массива"
                              {:tokens tokens})))))))))

;; Парсер для инициализации массивов
(defn- ^:private parse-array-initializer
  "Семантический парсинг инициализации массивов

  Грамматика:
  {val1, val2, val3, ...}

  Ключевые аспекты парсинга:
  - Распознавание списка значений
  - Поддержка различных типов значений
  - Проверка синтаксиса"
  [tokens]
  (log/debug "Начало парсинга инициализации массива")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[open-brace & rest] tokens]
    (when-not (= (:value open-brace) "{")
      (log/error "Ожидается '{' для инициализации массива")
      (throw (ex-info "Отсутствует '{' для инициализации"
                      {:tokens tokens})))
    
    (loop [remaining rest
           values []]
      (let [[current-token & next-tokens] remaining]
        (cond
          ;; Закрывающая фигурная скобка
          (= (:value current-token) "}")
          {:type :array-initializer
           :values values
           :tokens next-tokens}
          
          ;; Разделитель значений
          (= (:value current-token) ",")
          (recur next-tokens values)
          
          ;; Парсинг значения
          :else
          (let [value-expr (parse-expression (cons current-token remaining))
                [next-token & after-next] (:tokens value-expr)]
            (cond
              ;; Конец инициализации
              (= (:value next-token) "}")
              {:type :array-initializer
               :values (conj values value-expr)
               :tokens after-next}
              
              ;; Разделитель значений
              (= (:value next-token) ",")
              (recur after-next (conj values value-expr))
              
              :else
              (throw (ex-info "Некорректный синтаксис инициализации массива"
                              {:tokens tokens})))))))))

;; Парсер для структур
(defn parse-struct-declaration 
  "Семантический парсинг объявления структур

  Грамматика:
  struct identifier {
    type1 field1;
    type2 field2;
    ...
  };

  Ключевые аспекты парсинга:
  - Распознавание полей структуры
  - Поддержка вложенных типов
  - Проверка синтаксиса"
  [tokens]
  (log/debug "Начало парсинга объявления структуры")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[struct-token & rest] tokens]
    (when-not (= (:value struct-token) "struct")
      (log/error "Некорректное объявление структуры. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление структуры" 
                      {:tokens tokens})))
    
    (let [[name-token & after-name] rest]
      (when-not (= (:type name-token) :identifier)
        (log/error "Некорректное имя структуры")
        (throw (ex-info "Некорректное имя структуры"
                        {:tokens tokens})))
      
      (let [[open-brace & after-open] after-name]
        (when-not (= (:value open-brace) "{")
          (log/error "Ожидается '{' для объявления структуры")
          (throw (ex-info "Отсутствует '{' для структуры"
                          {:tokens tokens})))
        
        (loop [remaining after-open
               fields []]
          (let [[current-token & next-tokens] remaining]
            (cond
              ;; Закрывающая фигурная скобка
              (= (:value current-token) "}")
              {:type :struct-declaration
               :name (:value name-token)
               :fields fields
               :tokens next-tokens}
              
              ;; Объявление поля структуры
              (= (:type current-token) :type-keyword)
              (let [field-decl (parse-variable-declaration 
                                (cons current-token next-tokens))]
                (recur (:tokens field-decl) 
                       (conj fields field-decl)))
              
              ;; Пропуск разделителей
              (= (:value current-token) ";")
              (recur next-tokens fields)
              
              :else
              (throw (ex-info "Некорректный синтаксис объявления структуры"
                              {:tokens tokens})))))))))

;; Парсер для typedef
(defn parse-typedef 
  "Семантический парсинг typedef

  Грамматика:
  typedef type new_type_name;
  typedef struct identifier new_type_name;

  Ключевые аспекты парсинга:
  - Распознавание исходного и нового типов
  - Поддержка типов структур
  - Проверка синтаксиса"
  [tokens]
  (log/debug "Начало парсинга typedef")
  (log/trace "Входящие токены: " (pr-str (take 5 tokens)))

  (let [[typedef-token & rest] tokens]
    (when-not (= (:value typedef-token) "typedef")
      (log/error "Некорректное объявление typedef. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление typedef" 
                      {:tokens tokens})))
    
    (let [[first-token & next-tokens] rest]
      (cond
        ;; Простой typedef
        (= (:type first-token) :type-keyword)
        (let [[new-type-token & after-new] next-tokens]
          (when-not (= (:type new-type-token) :identifier)
            (log/error "Некорректное имя нового типа")
            (throw (ex-info "Некорректное имя нового типа"
                            {:tokens tokens})))
          
          (let [[semicolon & after-semicolon] after-new]
            (when-not (= (:value semicolon) ";")
              (log/error "Ожидается точка с запятой после typedef")
              (throw (ex-info "Отсутствует точка с запятой"
                              {:tokens tokens})))
            
            {:type :typedef
             :original-type (:value first-token)
             :new-type (:value new-type-token)
             :tokens after-semicolon}))
        
        ;; Typedef для структуры
        (= (:value first-token) "struct")
        (let [struct-decl (parse-struct-declaration 
                           (cons first-token next-tokens))
              [new-type-token & after-new] (:tokens struct-decl)]
          (when-not (= (:type new-type-token) :identifier)
            (log/error "Некорректное имя нового типа структуры")
            (throw (ex-info "Некорректное имя нового типа структуры"
                            {:tokens tokens})))
          
          (let [[semicolon & after-semicolon] after-new]
            (when-not (= (:value semicolon) ";")
              (log/error "Ожидается точка с запятой после typedef структуры")
              (throw (ex-info "Отсутствует точка с запятой"
                              {:tokens tokens})))
            
            {:type :typedef
             :original-type :struct
             :struct-definition struct-decl
             :new-type (:value new-type-token)
             :tokens after-semicolon}))
        
        :else
        (throw (ex-info "Некорректный синтаксис typedef"
                        {:tokens tokens}))))))

(defn parse-with-context [tokens context]
  (case (:memory-space context)
    :data   (parse-data-space tokens)
    :xdata  (parse-xdata-space tokens)
    :code   (parse-code-space tokens)
    (parse-default tokens)))

(defn tokenize-special-keywords [input]
  (recognize-memory-types input)
  (recognize-sfr-keywords input)
  (recognize-bit-addressing input))

(defn validate-8051-constraints [ast]
  (validate-memory-constraints ast)
  (validate-interrupt-vectors ast)
  (validate-register-usage ast))

(defn peek-tokens [tokens n]
  (take n tokens))

(defrecord ParsingContext [
  memory-space    ; текущее пространство памяти
  scope-type     ; тип области видимости
  interrupt-context ; контекст прерывания
])

(defn parse-bit-expression [tokens context]
  (if (in-sfr-context? context)
    (parse-bit-address tokens)
    (parse-bitwise-operation tokens)))

(defn tokenize-c51-specific [input]
  (let [base-tokens (lexer/tokenize input)]
    (-> base-tokens
        (lexer/recognize-memory-types)     ;; data, xdata, code
        (lexer/recognize-sfr-keywords)     ;; специальные регистры
        (lexer/recognize-bit-addressing)   ;; битовая адресация
        (lexer/recognize-interrupts))))    ;; прерывания

;; 1. Добавить специфичные для C51 токены в лексер:
(def c51-specific-tokens
  {:memory-types #{"data" "xdata" "code" "idata" "pdata"}
   :sfr-keywords #{"sfr" "sbit"}
   :bit-operators #{"^"}})

;; 2. Расширить функционал лексера:
(defn enhanced-tokenize [input]
  (-> input
      lexer/tokenize
      (lexer/enrich-with-c51-specifics)
      (lexer/validate-token-sequence)))

;; 3. Интегрировать лексер в парсер:
(defn parse [input]
  (let [tokens (enhanced-tokenize input)
        context (lexer/create-parsing-context tokens)]
    (parse-program tokens context)))

;; 1. Определить специфичные токены C51:
(def ^:private c51-token-types
  {:memory-directive :memory-directive    ;; data, xdata, code
   :sfr-declaration :sfr-declaration     ;; sfr, sbit
   :bit-address :bit-address            ;; P1^5
   :interrupt-vector :interrupt-vector}) ;; interrupt N

;; 2. Расширить лексер:
(defn tokenize-c51 [input]
  (let [base-tokens (lexer/tokenize input)]
    (->> base-tokens
         lexer/enrich-with-memory-directives
         lexer/enrich-with-sfr-declarations
         lexer/enrich-with-bit-addressing
         lexer/enrich-with-interrupts)))

;; 3. Интегрировать с парсером:
(defn parse-c51 [input]
  (let [tokens (tokenize-c51 input)
        context (->ParsingContext :default nil nil)]
    (parse-with-context tokens context)))

;; Функции для парсинга в разных пространствах памяти
(defn- ^:private parse-data-space 
  "Парсинг в пространстве памяти DATA"
  [tokens]
  (log/debug "Парсинг в пространстве DATA")
  ;; TODO: Реализовать специфичный парсинг для DATA
  (parse-program tokens))

(defn- ^:private parse-xdata-space 
  "Парсинг в пространстве памяти XDATA"
  [tokens]
  (log/debug "Парсинг в пространстве XDATA")
  ;; TODO: Реализовать специфичный парсинг для XDATA
  (parse-program tokens))

(defn- ^:private parse-code-space 
  "Парсинг в пространстве памяти CODE"
  [tokens]
  (log/debug "Парсинг в пространстве CODE")
  ;; TODO: Реализовать специфичный парсинг для CODE
  (parse-program tokens))

(defn- ^:private parse-default 
  "Парсинг по умолчанию"
  [tokens]
  (log/debug "Парсинг по умолчанию")
  (parse-program tokens))

;; Функции для распознавания специальных ключевых слов
(defn- ^:private recognize-memory-types 
  "Распознавание типов памяти"
  [input]
  (log/debug "Распознавание типов памяти")
  ;; TODO: Реализовать распознавание типов памяти
  input)

(defn- ^:private recognize-sfr-keywords 
  "Распознавание ключевых слов SFR"
  [input]
  (log/debug "Распознавание ключевых слов SFR")
  ;; TODO: Реализовать распознавание SFR
  input)

(defn- ^:private recognize-bit-addressing 
  "Распознавание битовой адресации"
  [input]
  (log/debug "Распознавание битовой адресации")
  ;; TODO: Реализовать распознавание битовой адресации
  input)

;; Функции для валидации ограничений 8051
(defn- ^:private validate-memory-constraints 
  "Валидация ограничений памяти"
  [ast]
  (log/debug "Валидация ограничений памяти")
  ;; TODO: Реализовать валидацию памяти
  ast)

(defn- ^:private validate-interrupt-vectors 
  "Валидация векторов прерываний"
  [ast]
  (log/debug "Валидация векторов прерываний")
  ;; TODO: Реализовать валидацию прерываний
  ast)

(defn- ^:private validate-register-usage 
  "Валидация использования регистров"
  [ast]
  (log/debug "Валидация использования регистров")
  ;; TODO: Реализовать валидацию регистров
  ast)

;; Функции для работы с SFR и битовой адресацией
(defn- ^:private in-sfr-context? 
  "Проверка контекста SFR"
  [context]
  (and context 
       (= (:scope-type context) :sfr)))

(defn- ^:private parse-bit-address 
  "Парсинг битовой адресации"
  [tokens]
  (log/debug "Парсинг битовой адресации")
  (let [[sfr-token & rest] tokens]
    (when-not (= (:type sfr-token) :identifier)
      (throw (ex-info "Ожидается идентификатор SFR" 
                      {:token sfr-token})))
    
    (let [[caret & after-caret] rest
          [bit-num & remaining] after-caret]
      (when-not (and (= (:value caret) "^")
                     (= (:type bit-num) :int_number))
        (throw (ex-info "Некорректный формат битовой адресации" 
                        {:tokens tokens})))
      
      {:type :bit-address
       :sfr (:value sfr-token)
       :bit (Integer/parseInt (:value bit-num))
       :tokens remaining})))