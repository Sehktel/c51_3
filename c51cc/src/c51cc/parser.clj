(ns c51cc.parser
  "Модуль для синтаксического анализатора"
  (:require [c51cc.lexer  :as lexer]
            [c51cc.logger :as log]
            [clojure.stacktrace :as stacktrace]
            [clojure.string :as str]
            ))

;; Парсер для языка C51 - абстракция синтаксического анализа

(declare internal-parse-parameters internal-parse-function-body)
(declare ast-node-types create-parser parse)

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
   :expression :expression
   :preprocessor-directive :preprocessor-directive
   :include-directive :include-directive})

;; Объявления функций для парсинга
(declare parser-parse-program)
(declare parser-parse-function-declaration)
(declare parser-parse-variable-declaration)
(declare parser-parse-expression)
(declare internal-parse-parameters)
(declare internal-parse-function-body)
(declare create-parser)
(declare parse)

;; Основная реализация парсера
(defrecord C51Parser [tokens])

(defn- ^:private internal-parse-parameters [tokens]
  "Семантический парсинг параметров функции"
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

(defn parser-parse-function-declaration 
  "Улучшенный парсинг объявления функции

  Расширенная грамматика:
  function-declaration ::= type-keyword identifier '(' parameters? ')' interrupt? interrupt-number? '{' function-body '}'

  Семантический анализ:
  - Гибкий парсинг параметров
  - Поддержка вложенных блоков
  - Робастная обработка ошибок
  - Поддержка прерываний"
  [parser tokens]
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

(defn- ^:private parse-variable-declaration [tokens]
  "Парсинг объявления переменной"
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

(defn- ^:private parse-expression [tokens]
  "Парсинг выражений с поддержкой сложных конструкций"
  (log/debug "Начало парсинга выражения")
  
  (let [[first-token & remaining] tokens]
    (cond
      (= (:type first-token) :identifier)
      {:type (:expression ast-node-types)
       :value (:value first-token)
       :tokens remaining}

      (= (:type first-token) :int_number)
      {:type (:expression ast-node-types)
       :value (:value first-token)
       :tokens remaining}

      (= (:type first-token) :operator)
      (let [left-expr (parse-expression remaining)
            [operator & after-op] (:tokens left-expr)
            right-expr (parse-expression after-op)]
        {:type (:expression ast-node-types)
         :value (str (:value left-expr) 
                     (:value operator) 
                     (:value right-expr))
         :tokens (:tokens right-expr)})
      
      :else
      (throw (ex-info "Неподдерживаемое выражение"
                      {:tokens tokens})))))

(defn- ^:private parse-assignment [tokens]
  "Семантический парсинг присваивания"
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

(defn- ^:private internal-parse-function-body [tokens]
  "Парсинг тела функции с расширенной диагностикой"
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
(defn parser-parse-program [parser tokens]
  "Семантический анализ программы с созданием полноценного AST"
  (log/debug "Начало семантического парсинга программы")
  
  (loop [remaining-tokens tokens
         parsed-nodes []]
    (if (empty? remaining-tokens)
      {:type (:program ast-node-types)
       :nodes parsed-nodes}
      (let [result (parser-parse-function-declaration parser remaining-tokens)
            node (dissoc result :tokens)]
        (recur (:tokens result) (conj parsed-nodes node))))))

(defn parse
  "Основная функция парсинга с семантическим анализом

   Ключевые особенности:
   - Полный семантический анализ токенов
   - Создание абстрактного синтаксического дерева (AST)
   - Гибкая обработка различных конструкций языка"
  [tokens]  
  (log/debug "Начало семантического парсинга программы")
  (parser-parse-program (C51Parser. tokens) tokens))
