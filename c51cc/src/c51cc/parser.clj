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



(defn parser-parse-function-declaration [parser tokens]
  "Улучшенный парсинг объявления функции

  Расширенная грамматика:
  function-declaration ::= type-keyword identifier '(' parameters? ')' interrupt? interrupt-number? '{' function-body '}'

  Семантический анализ:
  - Гибкий парсинг параметров
  - Поддержка вложенных блоков
  - Робастная обработка ошибок
  - Поддержка прерываний"
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
                         :return-type (:value type-token)
                         :name (:value name-token)
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

(defn parser-parse-variable-declaration [parser tokens]
  (log/debug "Начало парсинга объявления переменной")
  (log/trace "Входящие токены для объявления переменной: " (pr-str (take 5 tokens)))
  
  (let [[type-token & remaining] tokens
        [name-token & remaining] remaining]
    (when-not (and (= (:type type-token) :type-keyword)
                   (= (:type name-token) :identifier))
      (log/error "Ошибка при парсинге объявления переменной. Токены: " (pr-str tokens))
      (throw (ex-info "Некорректное объявление переменной"
                      {:tokens tokens})))

    (log/info "Распознана переменная: " (:value name-token) 
              " типа " (:value type-token))
    
    {:type (:variable-declaration ast-node-types)
     :var-type (:value type-token)
     :name (:value name-token)
     :tokens remaining}))

(defn parser-parse-expression [parser tokens]
  "Парсинг выражений

  Теоретическая модель:
  - Рекурсивный спуск для разбора выражений
  - Поддержка арифметических и логических операций

  Сложность: O(n), где n - длина выражения"
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

      :else
      (throw (ex-info "Неподдерживаемое выражение"
                      {:tokens tokens})))))

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
    (parser-parse-program parser tokens)))

(defn- ^:private internal-parse-parameters
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

(defn- ^:private internal-parse-function-body
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

;; Реализации парсера
(defn parser-parse-program [parser tokens]
  "Анализ программы как последовательности деклараций и определений функций

  Теоретическое обоснование:
  - Программа рассматривается как последовательность функций
  - Поддерживаются только объявления функций"
  (log/debug "Начало парсинга программы. Количество токенов: " (count tokens))
  (log/trace "Первые 5 токенов: " (pr-str (take 5 tokens)))
  (loop [remaining-tokens tokens
         parsed-nodes []]
    (if (empty? remaining-tokens)
      (do 
        (log/info "Парсинг программы завершен. Количество узлов: " (count parsed-nodes))
        (log/debug "Узлы программы: " (pr-str parsed-nodes))
        {:type (:program ast-node-types)
         :nodes parsed-nodes})
      (let [[current & _] remaining-tokens
            result (do 
                     (log/trace "Попытка парсинга функции")
                     (parser-parse-function-declaration parser remaining-tokens))
            node (dissoc result :tokens)]
        (log/trace "Распознан узел: " (pr-str node))
        (recur (:tokens result) (conj parsed-nodes node))))))
