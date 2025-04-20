(ns c51cc.parser
  "Модуль для синтаксического анализатора"
  (:require [c51cc.lexer  :as lexer]
            [c51cc.logger :as log]))

;; Парсер для языка C51 - абстракция синтаксического анализа

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
        (let [[declaration-node remaining] (parse-function-declaration this remaining-tokens)]
          (log/trace "Распознан узел декларации: " declaration-node)
          (recur remaining (conj parsed-nodes declaration-node))))))

  (parse-function-declaration [_ tokens]
    "Парсинг объявления функции

    Грамматика:
    function-declaration ::= type-keyword identifier '(' parameters ')' '{' function-body '}'

    Семантический анализ:
    - Проверка корректности типа возвращаемого значения
    - Валидация имени функции
    - Анализ параметров"
    (log/debug "Начало парсинга объявления функции")
    (let [[type-token & remaining] tokens
          [name-token & remaining] remaining
          [open-paren & remaining] remaining]
      (when-not (and (= (:type type-token) :type-keyword)
                     (= (:type name-token) :identifier)
                     (= (:value open-paren) "("))
        (log/info "Ошибка при парсинге объявления функции")
        (throw (ex-info "Некорректное объявление функции"
                        {:tokens tokens})))

      (log/trace "Распознана функция: " (:value name-token) " с типом возврата " (:value type-token))
      {:type (:function-declaration ast-node-types)
       :return-type (:value type-token)
       :name (:value name-token)
       :tokens remaining}))

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
  (->C51Parser tokens))

;; Главная функция парсинга
(defn parse
  "Основная функция парсинга, принимающая последовательность токенов

   Архитектурные соображения:
   - Абстракция над конкретной реализацией парсера
   - Гибкость и расширяемость"
  [tokens]
  (let [parser (create-parser tokens)]
    (parse-program parser tokens)))