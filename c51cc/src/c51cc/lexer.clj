(ns c51cc.lexer
  "Модуль для лексического анализатора"
  (:require [clojure.string :as str]
            [c51cc.logger :as log]))

;; Предварительное объявление всех функций
(declare  keywords
          is-special-keyword?
          is-type-keyword?
          is-separator-keyword?
          is-operator-keyword?
          is-control-flow-keyword?
          is-constant-keyword?
          is-identifier?
          is-number?
          is-string?
          get-token-type
          tokenize)

;; =============================================
;; Определение типов токенов
;; =============================================

;; Определение типов токенов
(def keywords
  "Определение типов токенов"
  {
   ;; Preprocessor Directives
   :include_directive ["#include"]
   ;; Системные заголовочные файлы в угловых скобках: <stdio.h>
   :include_path_system #"^<[a-zA-Z0-9_./]+\.h>"
   ;; Пользовательские заголовочные файлы в двойных кавычках: "stdio.h"
   ;; TODO: Добавить поддержку пользовательских заголовочных файлов в двойных кавычках
   ;; :include_path_user #"^\"[a-zA-Z0-9_./]+\.h\""
   :include_path_regex #"<[a-zA-Z0-9_./]+\.h>"
   :include_path_h_regex #"<[a-zA-Z0-9_./]+\.h>"

   ;; Директивы препроцессора
   :define_directive ["#define"]
   :undef_directive ["#undef"]
   :if_directive ["#if"]
   :ifdef_directive ["#ifdef"]
   :ifndef_directive ["#ifndef"]
   :else_directive ["#else"]
   :elif_directive ["#elif"]
   :endif_directive ["#endif"]
   :error_directive ["#error"]
   
   ;; TODO: Добавить поддержку директивы #pragma, #line, #warning
   ;; :pragma_directive ["#pragma"]
   ;; :line_directive ["#line"]
   ;; :warning_directive ["#warning"]
   
   
   ;; Специальные ключевые слова
   :sfr_special_keyword ["sfr"]  ;; Special Function Register
   :sbit_special_keyword ["sbit"] ;; Special Bit
   :interrupt_special_keyword ["interrupt"] ;; Interrupt
   :using_special_keyword ["using"] ;; Using

   ;; Типы данных
   :char_type_keyword ["char"] ;; 1 байт
   :int_type_keyword ["int"] ;; 2 байта
   :void_type_keyword ["void"] ;; Пустой тип
   :signed_type_keyword ["signed"] ;; Знаковый тип
   :unsigned_type_keyword ["unsigned"] ;; Беззнаковый тип

   ;; Разделители
   :open_round_bracket ["("] ;; Открывающая круглая скобка
   :close_round_bracket [")"] ;; Закрывающая круглая скобка
   :open_curly_bracket ["{"] ;; Открывающая фигурная скобка
   :close_curly_bracket ["}"] ;; Закрывающая фигурная скобка
   :open_square_bracket ["["] ;; Открывающая квадратная скобка
   :close_square_bracket ["]"] ;; Закрывающая квадратная скобка
   :semicolon [";"] ;; Точка с запятой
   :comma [","] ;; Запятая
   :colon [":"] ;; Двоеточие

   ;; Арифметические операторы
   :arithmetic_operators ["+", "-", "*", "/", "%"] ;; Арифметические операторы
   ;; Операторы сравнения
   :comparison_operators ["==", "!=", "<", ">", "<=", ">="] 
   ;; Логические операторы
   :logical_operators ["&&", "||", "!"] 
   ;; Побитовые операторы
   :bitwise_operators ["&", "|", "^"] 
   ;; Битовые сдвиговые операторы
   :bitwise_shift_operators ["<<", ">>"] 
   ;; Операторы присваивания
   :assignment_operators ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^="]
   ;; Операторы инкремента и декремента
   :inc_operator ["++"]
   :dec_operator ["--"]
   :increment_decrement_operators ["++", "--"]
   ;; Унарные операторы
   :unary_operators ["~", "!"]

   ;; Управляющие конструкции
   :if_keyword ["if"]
   :else_keyword ["else"]
   :switch_keyword ["switch"]
   :case_keyword ["case"] ;; TODO: Добавить поддержку case
   :default_keyword ["default"] ;; TODO: Добавить поддержку default
   :for_keyword ["for"]
   :while_keyword ["while"]
   :do_keyword ["do"]
   :break_keyword ["break"]
   :continue_keyword ["continue"]
   :return_keyword ["return"]
   :goto_keyword ["goto"]

   ;; Константы
   :const_keyword ["const"]

   ;; Идентификаторы
   :identifier #"[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*"
   ;; Целочисленные константы
   :int_number #"[-+]?[0-9]+"
   ;; Шестнадцатеричные константы
   :hex_number #"0[xX][0-9a-fA-F]+"
   ;; Строковые константы
   :string #"\"[^\"]*\""
   })

;; ;; Функция для проверки, является ли объект регулярным выражением
;; (defn regexp? [x]
;;   (instance? java.util.regex.Pattern x))

;; =============================================
;; Проверки типов токенов
;; =============================================

;; Проверяет, является ли токен препроцессорной директивой
(defn is-preprocessor-directive?
  "Проверяет, является ли токен препроцессорной директивой"
  [s]
  (boolean (some #{s} 
                 (concat
                  (get keywords :include_directive)
                  (get keywords :define_directive)
                  (get keywords :undef_directive)
                  (get keywords :if_directive)
                  (get keywords :ifdef_directive)
                  (get keywords :ifndef_directive)
                  (get keywords :else_directive)
                  (get keywords :elif_directive)
                  (get keywords :endif_directive)
                  (get keywords :error_directive)
                  ;; (get keywords :pragma_directive)
                  ;; (get keywords :line_directive)
                  ;; (get keywords :warning_directive)
                  ))))

;; Проверяет, является ли токен путем для включения файла
(defn is-include-path?
  "Проверяет, является ли токен путем для включения файла.
   Допускаются только системные заголовочные файлы в формате <file.h>"
  [s]
  ;; TODO: Добавить поддержку пользовательских заголовочных файлов в двойных кавычках
  (boolean (re-matches (:include_path_system keywords) s)))

;; Проверяет, является ли токен специальным ключевым словом
(defn is-special-keyword?
  "Проверяет, является ли токен специальным ключевым словом"
  [s]
  (boolean (some #{s}
                 (concat
                  (get keywords :sfr_special_keyword)
                  (get keywords :sbit_special_keyword)
                  (get keywords :interrupt_special_keyword)
                  (get keywords :using_special_keyword)))))

;; Проверяет, является ли токен типом
(defn is-type-keyword?
  "Проверяет, является ли токен типом"
  [s]
  (boolean (some #{s}
                 (concat
                  (get keywords :char_type_keyword)
                  (get keywords :int_type_keyword)
                  (get keywords :void_type_keyword)
                  (get keywords :signed_type_keyword)
                  (get keywords :unsigned_type_keyword)))))

;; Проверяет, является ли токен разделителем
(defn is-separator-keyword?
  "Проверяет, является ли токен разделителем"
  [s]
  (boolean (some #{s}
                 (concat
                  (get keywords :open_round_bracket)
                  (get keywords :close_round_bracket)
                  (get keywords :open_curly_bracket)
                  (get keywords :close_curly_bracket)
                  (get keywords :open_square_bracket)
                  (get keywords :close_square_bracket)
                  (get keywords :semicolon)
                  (get keywords :comma)
                  (get keywords :colon)))))

;; Проверяет, является ли токен оператором
(defn is-operator-keyword?
  "Проверяет, является ли токен оператором"
  [s]
  (boolean (some #{s}
                 (concat
                  (get keywords :arithmetic_operators)
                  (get keywords :comparison_operators)
                  (get keywords :logical_operators)
                  (get keywords :bitwise_operators)
                  (get keywords :bitwise_shift_operators)
                  (get keywords :assignment_operators)
                  (get keywords :increment_decrement_operators)
                  (get keywords :unary_operators)
                  (get keywords :inc_operator)
                  (get keywords :dec_operator)))))

;; Проверяет, является ли токен управляющей конструкцией
(defn is-control-flow-keyword?
  "Проверяет, является ли токен управляющей конструкцией"
  [s]
  (boolean (some #{s}
                 (concat
                  (get keywords :if_keyword)
                  (get keywords :else_keyword)
                  (get keywords :switch_keyword)
                  (get keywords :case_keyword) ;; TODO: Добавить поддержку case
                  (get keywords :default_keyword) ;; TODO: Добавить поддержку default
                  (get keywords :for_keyword)
                  (get keywords :while_keyword)
                  (get keywords :do_keyword)
                  (get keywords :break_keyword)
                  (get keywords :continue_keyword)
                  (get keywords :return_keyword)
                  (get keywords :goto_keyword)))))

;; Проверяет, является ли токен константой
(defn is-constant-keyword?
  "Проверяет, является ли токен константой"
  [s]
  (boolean (some #{s} (get keywords :const_keyword))))

;; Проверяет, является ли токен идентификатором
(defn is-identifier?
  "Проверяет, является ли токен идентификатором"
  [s]
  (boolean (re-matches (:identifier keywords) s)))

;; Проверяет, является ли токен числом
(defn is-number?
  "Проверяет, является ли токен числом"
  [s]
  (boolean
   (or
    (re-matches (:int_number keywords) s)
    (re-matches (:hex_number keywords) s))))

;; Определяет тип числа
(defn get-number-type
  "Определяет тип числа"
  [s]
  (cond
    (re-matches (:int_number keywords) s) :int_number
    (re-matches (:hex_number keywords) s) :hex_number
    :else :unknown))

;; Проверяет, является ли токен строковым литералом
(defn is-string?
  "Проверяет, является ли токен строковым литералом"
  [s]
  (boolean (re-matches (:string keywords) s)))

;; Определяет тип токена
(defn get-token-type
  "Определяет тип токена"
  [token]
  (cond
    (is-preprocessor-directive? token) :preprocessor-directive
    (is-include-path? token) :include-path
    (is-special-keyword? token) :special-keyword
    (is-type-keyword? token) :type-keyword
    (is-separator-keyword? token) :separator
    (is-operator-keyword? token) :operator
    (is-control-flow-keyword? token) :control-flow
    (is-constant-keyword? token) :constant
    (is-identifier? token) :identifier
    (is-number? token) (get-number-type token)
    (is-string? token) :string
    :else :unknown))

;; =============================================
;; Токенизация кода
;; =============================================

;; Создает токен для ключевого слова с указанным типом
(defn token-for-keyword
  "Создает токен для ключевого слова с указанным типом"
 [token keyword-type]
 {:value token :type keyword-type})

;; Создает карту токенов для быстрого сопоставления
(defn create-token-map
  "Создает карту токенов для быстрого сопоставления"
  []
  (log/debug "Создание карты токенов")
  (let [keyword-type-mapping {
        ;; Special Keywords
        :sfr_special_keyword :special-keyword
        :sbit_special_keyword :special-keyword
        :interrupt_special_keyword :special-keyword
        :using_special_keyword :special-keyword

        ;; Data Types
        :char_type_keyword :type-keyword
        :int_type_keyword :type-keyword
        :void_type_keyword :type-keyword
        :signed_type_keyword :type-keyword
        :unsigned_type_keyword :type-keyword

        ;; Separators
        :open_round_bracket :separator
        :close_round_bracket :separator
        :open_curly_bracket :separator
        :close_curly_bracket :separator
        :open_square_bracket :separator
        :close_square_bracket :separator
        :semicolon :separator
        :comma :separator
        :colon :separator

        ;; Operators
        :arithmetic_operators :operator
        :comparison_operators :operator
        :logical_operators :operator
        :bitwise_operators :operator
        :bitwise_shift_operators :operator
        :assignment_operators :operator
        :inc_operator :operator
        :dec_operator :operator
        :increment_decrement_operators :operator
        :unary_operators :operator

        ;; Control Flow
        :if_keyword :control-flow
        :else_keyword :control-flow
        :switch_keyword :control-flow
        :case_keyword :control-flow
        :default_keyword :control-flow
        :for_keyword :control-flow
        :while_keyword :control-flow
        :do_keyword :control-flow
        :break_keyword :control-flow
        :continue_keyword :control-flow
        :return_keyword :control-flow
        :goto_keyword :control-flow

        ;; Constants
        :const_keyword :constant}
        result (->> (dissoc keywords 
                           :identifier 
                           :int_number 
                           :hex_number 
                           :string 
                           :include_path_system
                           :include_path_user)
                   (mapcat (fn [[k v]]
                           (when (vector? v)
                             (map #(vector % (token-for-keyword % (get keyword-type-mapping k k))) v))))
                   (into {})
                   ;; Сортируем по длине токена (от большего к меньшему) для корректного совпадения
                   (sort-by #(- (count (first %)))))]
    (log/trace "Создана карта токенов. Количество токенов: " (count result))
    result))

;; Находит токен на основе регулярных выражений
(defn find-regex-token
  "Находит токен на основе регулярных выражений"
  [code]
  (log/trace "Поиск токена по регулярным выражениям. Код: " code)
  (cond
    ;; Проверяем системные пути включения (<stdio.h>)
    (re-find (:include_path_system keywords) code)
    (let [include-match (re-find (:include_path_system keywords) code)]
      (log/debug "Найден системный путь включения: " include-match)
      {:value include-match
       :type :include-path})

    ;; Проверяем шестнадцатеричные числа
    (re-find #"^0[xX][0-9a-fA-F]+" code)
    (let [hex-match (re-find #"^0[xX][0-9a-fA-F]+" code)]
      (log/debug "Найдено шестнадцатеричное число: " hex-match)
      {:value hex-match
       :type :hex_number})

    ;; Проверяем целые числа (без плавающей точки)
    (re-find #"^[-+]?[0-9]+" code)
    (let [int-match (re-find #"^[-+]?[0-9]+" code)]
      (log/debug "Найдено целое число: " int-match)
      {:value int-match
       :type :int_number})

    ;; Проверяем строки
    (and (str/starts-with? code "\"")
         (> (count code) 1)
         (re-find #"^\"[^\"]*\"" code))
    (let [string-match (re-find #"^\"[^\"]*\"" code)]
      (log/debug "Найдена строка: " string-match)
      {:value string-match
       :type :string})

    ;; Проверяем идентификаторы
    (re-find #"^[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*" code)
    (let [identifier-match (re-find #"^[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*" code)]
      (log/debug "Найден идентификатор: " identifier-match)
      {:value identifier-match
       :type :identifier})

    :else 
    (do 
      (log/info "Не удалось распознать токен")
      nil)))

;; Токенизирует исходный код
(defn tokenize
  "Преобразует исходный код в последовательность токенов"
  [code]
  (log/debug "Начало токенизации исходного кода. Длина кода: " (count code))
  (let [token-map (create-token-map)
        input-str (cond
                   (string? code) code
                   (sequential? code) (str/join "\n" (map str code))
                   :else (str code))]
    (loop [remaining-code (str/trim input-str)
           tokens []]
      (if (str/blank? remaining-code)
        (do 
          (log/info "Токенизация завершена. Количество токенов: " (count tokens))
          tokens)
        (let [;; Сначала проверяем на системные пути включения
              include-match (when (str/starts-with? remaining-code "<")
                            (re-find (:include_path_system keywords) remaining-code))
              
              ;; Если не системный путь включения, пытаемся найти точное совпадение токена
              exact-match (when-not include-match
                           (some (fn [[token token-info]]
                                  (when (str/starts-with? remaining-code token)
                                    token-info))
                                token-map))

              ;; Если нет точного совпадения, пытаемся найти по регулярным выражениям
              token (cond
                     include-match {:value (str include-match) :type :include-path}
                     exact-match exact-match
                     :else (find-regex-token remaining-code))]

          (if token
            (let [value (:value token)
                  token-length (if (string? value)
                                (count value)
                                ;; Для случая, когда value не строка (возвращаемое значение из re-find может быть вектором)
                                (count (str (first (if (vector? value) value [value])))))]
              (log/trace "Распознан токен: " token)
              (recur
               (str/trim (subs remaining-code token-length))
               (conj tokens (if (vector? (:value token))
                            ;; Если значение - вектор, берем первый элемент (полное совпадение)
                            (assoc token :value (str (first (:value token))))
                            (update token :value str)))))
            (do 
              (log/info "Ошибка токенизации. Оставшийся код: " remaining-code)
              (throw (ex-info "Tokenization error" {:remaining-code remaining-code})))))))))

;; =============================================
;; Обогащение токенов C51-специфичной информацией
;; =============================================

;; Пространства памяти C51
(def ^:private c51-memory-spaces
  "Пространства памяти C51"
  #{"data" "xdata" "code" "idata" "pdata"})

;; Специальные регистры C51
(def ^:private c51-sfr-keywords
  "Ключевые слова для специальных регистров"
  #{"sfr" "sbit"})

;; Обогащает токены информацией о директивах памяти
(defn enrich-with-memory-directives
  "Обогащает токены информацией о директивах памяти"
  [tokens]
  (map (fn [token]
         (if (contains? c51-memory-spaces (:value token))
           (assoc token 
                  :type :memory-space
                  :memory-type (keyword (:value token)))
           token))
       tokens))

;; Обогащает токены информацией о специальных регистрах
(defn enrich-with-sfr-declarations
  "Обогащает токены информацией о специальных регистрах"
  [tokens]
  (map (fn [token]
         (if (contains? c51-sfr-keywords (:value token))
           (assoc token 
                  :type :sfr-keyword
                  :sfr-type (keyword (:value token)))
           token))
       tokens))

;; Обогащает токены информацией о битовой адресации
(defn enrich-with-bit-addressing
  "Обогащает токены информацией о битовой адресации"
  [tokens]
  (loop [remaining tokens
         result []]
    (let [[curr next after] (take 3 remaining)]
      (cond
        (empty? remaining)
        result
        
        ;; Проверяем паттерн битовой адресации: reg^bit
        (and curr next after
             (= (:type curr) :identifier)
             (= (:value next) "^")
             (= (:type after) :int_number))
        (recur (drop 3 remaining)
               (conj result
                     {:type :bit-address
                      :value (str (:value curr) "^" (:value after))
                      :register (:value curr)
                      :bit (Integer/parseInt (:value after))}))
        
        :else
        (recur (rest remaining)
               (conj result curr))))))

;; Обогащает токены информацией о прерываниях
(defn enrich-with-interrupts
  "Обогащает токены информацией о прерываниях"
  [tokens]
  (loop [remaining tokens
         result []]
    (let [[curr next] (take 2 remaining)]
      (cond
        (empty? remaining)
        result
        
        ;; Проверяем паттерн прерывания: interrupt N
        (and curr next
             (= (:value curr) "interrupt")
             (= (:type next) :int_number))
        (recur (drop 2 remaining)
               (conj result
                     {:type :interrupt
                      :value (str "interrupt " (:value next))
                      :number (Integer/parseInt (:value next))}))
        
        :else
        (recur (rest remaining)
               (conj result curr))))))

;; Обогащает токены всей C51-специфичной информацией
(defn enrich-with-c51-specifics
  "Обогащает токены всей C51-специфичной информацией"
  [tokens]
  (-> tokens
      enrich-with-memory-directives
      enrich-with-sfr-declarations
      enrich-with-bit-addressing
      enrich-with-interrupts))

;; =============================================
;; Токенизация C51
;; =============================================

;; Токенизирует и обогащает токены специфичной для C51 информацией
(defn tokenize-c51
  "Токенизация и обогащение токенов специфичной для C51 информацией"
  [input]
  (log/debug "Начало токенизации C51 кода")
  (let [base-tokens (tokenize input)]
    (log/debug "Базовая токенизация завершена, обогащаем токены")
    (->> base-tokens
         enrich-with-c51-specifics)))

;; Проверяет корректность последовательности токенов
(defn validate-token-sequence
  "Проверяет корректность последовательности токенов"
  [tokens]
  (log/debug "Валидация последовательности токенов")
  (doseq [[prev curr] (partition 2 1 tokens)]
    (when (and (= (:type prev) :operator)
               (= (:type curr) :operator)
               (not (#{"+" "-" "&" "|" "^"} (:value curr))))
      (throw (ex-info "Недопустимая последовательность операторов"
                     {:prev prev :curr curr}))))
  tokens)

;; C51 специфичные функции лексера
(defn recognize-memory-types
  "Распознавание типов памяти (data, xdata, code)"
  [tokens]
  (log/debug "Распознавание типов памяти")
  tokens)

;; Распознавание ключевых слов SFR
(defn recognize-sfr-keywords
  "Распознавание ключевых слов SFR"
  [tokens]
  (log/debug "Распознавание SFR")
  tokens)

;; Распознавание битовой адресации
(defn recognize-bit-addressing
  "Распознавание битовой адресации"
  [tokens]
  (log/debug "Распознавание битовой адресации")
  tokens)

;; Распознавание прерываний
(defn recognize-interrupts
  "Распознавание прерываний"
  [tokens]
  (log/debug "Распознавание прерываний")
  tokens)

;; Создание контекста парсинга
(defn create-parsing-context
  "Создание контекста парсинга"
  [tokens]
  (log/debug "Создание контекста парсинга")
  {:scope-type :default
   :memory-space :default
   :tokens tokens})