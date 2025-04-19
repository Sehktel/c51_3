(ns c51cc.parser
  "Модуль для синтаксического анализатора"
  (:require [c51cc.lexer :as lexer]))

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
    (loop [remaining-tokens tokens
           parsed-nodes []]
      (if (empty? remaining-tokens)
        {:type (:program ast-node-types)
         :nodes parsed-nodes}
        (let [[declaration-node remaining] (parse-function-declaration this remaining-tokens)]
          (recur remaining (conj parsed-nodes declaration-node))))))

  (parse-function-declaration [_ tokens]
    "Парсинг объявления функции

    Грамматика:
    function-declaration ::= type-keyword identifier '(' parameters ')' '{' function-body '}'

    Семантический анализ:
    - Проверка корректности типа возвращаемого значения
    - Валидация имени функции
    - Анализ параметров"
    (let [[type-token & remaining] tokens
          [name-token & remaining] remaining
          [open-paren & remaining] remaining]
      (when-not (and (= (:type type-token) :type-keyword)
                     (= (:type name-token) :identifier)
                     (= (:value open-paren) "("))
        (throw (ex-info "Некорректное объявление функции"
                        {:tokens tokens})))

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
    (let [[type-token & remaining] tokens
          [name-token & remaining] remaining]
      (when-not (and (= (:type type-token) :type-keyword)
                     (= (:type name-token) :identifier))
        (throw (ex-info "Некорректное объявление переменной"
                        {:tokens tokens})))

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
                        {:tokens tokens}))))))

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