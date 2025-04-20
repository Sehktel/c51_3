(ns c51cc.parser
  "Модуль для синтаксического анализатора"
  (:require [c51cc.lexer  :as lexer]
            [c51cc.logger :as log]))

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
   :expression :expression})

;; Протокол для парсинга различных конструкций языка
(defprotocol ASTParser
  "Протокол для парсинга различных синтаксических конструкций"
  (parse-program [this tokens] "Парсинг всей программы")
  (parse-function-declaration [this tokens] "Парсинг объявления функции")
  (parse-variable-declaration [this tokens] "Парсинг объявления переменной")
  (parse-expression [this tokens] "Парсинг выражения"))

;; Основная реализация парсера
(defrecord C51Parser [tokens]
  ASTParser

  (parse-program [this tokens]
    "Анализ программы как последовательности деклараций и определений

    Теоретическое обоснование:
    - Программа рассматривается как последовательность верхнеуровневых конструкций
    - Используется рекурсивный спуск для декомпозиции структуры

    Сложность: O(n), где n - количество токенов"
    (log/debug "Начало парсинга программы. Количество токенов: " (count tokens))
    (loop [remaining-tokens tokens
           parsed-nodes []]
      (if (empty? remaining-tokens)
        (do 
          (log/info "Парсинг программы завершен. Количество узлов: " (count parsed-nodes))
          {:type (:program ast-node-types)
           :nodes parsed-nodes})
        (let [declaration-result (parse-function-declaration this remaining-tokens)
              declaration-node (:type declaration-result)
              remaining (get declaration-result :tokens)]
          (log/trace "Распознан узел декларации: " declaration-node)
          (recur remaining (conj parsed-nodes declaration-node))))))

  (parse-function-declaration [_ tokens]
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
            [open-brace & after-brace] (:tokens parameters-result)
            body-result (parse-function-body after-brace)]
        
        (when-not (= (:value open-brace) "{")
          (throw (ex-info "Ожидается открывающая фигурная скобка" {})))
        
        (log/trace "Распознана функция:" (:value name-token))
        
        {:type (:function-declaration ast-node-types)
         :return-type (:value type-token)
         :name (:value name-token)
         :parameters (:parameters parameters-result)
         :body (:body body-result)
         :tokens (:tokens body-result)})))

  (parse-variable-declaration [_ tokens]
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

  (parse-expression [_ tokens]
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
  "Парсинг параметров функции"
  [tokens]
  (log/debug "Начало парсинга параметров функции")
  (log/trace "Токены для парсинга параметров: " (pr-str tokens))
  (loop [remaining tokens
         parameters []]
    (let [[current-token & rest] remaining]
      (log/trace "Текущий токен: " (pr-str current-token))
      (log/trace "Оставшиеся токены: " (pr-str rest))
      (cond 
        (= (:value current-token) ")") 
        (do 
          (log/trace "Завершение парсинга параметров. Найдено параметров: " (count parameters))
          {:parameters parameters 
           :tokens rest})
        
        (= (:type current-token) :type-keyword)
        (let [[name-token & next-tokens] rest]
          (when-not (= (:type name-token) :identifier)
            (throw (ex-info "Некорректное имя параметра" 
                             {:token name-token})))
          (log/trace "Распознан параметр: тип " (:value current-token) ", имя " (:value name-token))
          (recur next-tokens 
                 (conj parameters 
                       {:type (:value current-token)
                        :name (:value name-token)})))
        
        :else 
        (throw (ex-info "Неожиданный токен в списке параметров" 
                        {:token current-token}))))))

(defn- parse-function-body
  "Парсинг тела функции"
  [tokens]
  (loop [remaining tokens
         depth 1
         body-tokens []]
    (let [[current-token & rest] remaining]
      (cond 
        (nil? current-token) 
        (throw (ex-info "Незавершенное тело функции" {}))
        
        (= (:value current-token) "{") 
        (recur rest (inc depth) (conj body-tokens current-token))
        
        (= (:value current-token) "}") 
        (if (= depth 1)
          {:body body-tokens 
           :tokens rest}
          (recur rest (dec depth) (conj body-tokens current-token)))
        
        :else 
        (recur rest depth (conj body-tokens current-token))))))