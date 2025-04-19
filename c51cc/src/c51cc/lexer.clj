(ns c51cc.lexer
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

;; Предварительное объявление всех функций
(declare keywords)
(declare is-keyword?)
(declare is-special-keyword?)
(declare is-type-keyword?)
(declare is-separator-keyword?)
(declare is-operator-keyword?)
(declare is-control-flow-keyword?)
(declare is-constant-keyword?)
(declare is-identifier?)
(declare is-number?)
(declare is-string?)
(declare get-token-type)
(declare tokenize)

;; =============================================
;; Определение типов токенов
;; =============================================

(def keywords
  {
   ;; Special Keywords    
   :sfr_special_keyword ["sfr"]
   :sbit_special_keyword ["sbit"]
   :interrupt_special_keyword ["interrupt"]
   :using_special_keyword ["using"]
   
   ;; Data Types
   :char_type_keyword ["char"]
   :int_type_keyword ["int"]
   :void_type_keyword ["void"]
   :signed_type_keyword ["signed"]
   :unsigned_type_keyword ["unsigned"]

   ;; Separators
   :open_round_bracket ["("]
   :close_round_bracket [")"]
   :open_curly_bracket ["{"]
   :close_curly_bracket ["}"]
   :open_square_bracket ["["]
   :close_square_bracket ["]"] 
   :semicolon [";"]
   :comma [","]
   :colon [":"]   

   ;; Operators
   :arithmetic_operators ["+", "-", "*", "/", "%"]
   :comparison_operators ["==", "!=", "<", ">", "<=", ">="]
   :logical_operators ["&&", "||", "!"]
   :bitwise_operators ["&", "|", "^"]
   :bitwise_shift_operators ["<<", ">>"]
   :assignment_operators ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^="]
   :inc_operator ["++"]
   :dec_operator ["--"]
   :increment_decrement_operators ["++", "--"]
   :unary_operators ["~", "!"]

   ;; Control Flow:
   :if_keyword ["if"]
   :else_keyword ["else"]
   :switch_keyword ["switch"]
   :case_keyword ["case"]
   :default_keyword ["default"]
   :for_keyword ["for"]
   :while_keyword ["while"]
   :do_keyword ["do"]
   :break_keyword ["break"]
   :continue_keyword ["continue"]
   :return_keyword ["return"]
   :goto_keyword ["goto"]
   
   ;; Constants
   :const_keyword ["const"]

   ;; Identifiers
   :identifier #"[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*"
   :int_number #"[-+]?[0-9]+"
   :hex_number #"0[xX][0-9a-fA-F]+"
   
   :string #"\"[^\"]*\""
   })

;; Функция для проверки, является ли объект регулярным выражением
(defn regexp? [x]
  (instance? java.util.regex.Pattern x))

;; =============================================
;; Проверки типов токенов
;; =============================================

(defn is-special-keyword? [s]
  (boolean (some #{s} 
                 (concat 
                  (get keywords :sfr_special_keyword)
                  (get keywords :sbit_special_keyword)
                  (get keywords :interrupt_special_keyword)
                  (get keywords :using_special_keyword)))))

(defn is-type-keyword? [s]
  (boolean (some #{s} 
                 (concat 
                  (get keywords :char_type_keyword)
                  (get keywords :int_type_keyword)
                  (get keywords :void_type_keyword)
                  (get keywords :signed_type_keyword)
                  (get keywords :unsigned_type_keyword)))))

(defn is-separator-keyword? [s]
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

(defn is-operator-keyword? [s]
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

(defn is-control-flow-keyword? [s]
  (boolean (some #{s} 
                 (concat 
                  (get keywords :if_keyword)
                  (get keywords :else_keyword)
                  (get keywords :switch_keyword)
                  (get keywords :case_keyword)
                  (get keywords :default_keyword)
                  (get keywords :for_keyword)
                  (get keywords :while_keyword)
                  (get keywords :do_keyword)
                  (get keywords :break_keyword)
                  (get keywords :continue_keyword)
                  (get keywords :return_keyword)
                  (get keywords :goto_keyword)))))

(defn is-constant-keyword? [s]
  (boolean (some #{s} (get keywords :const_keyword))))

(defn is-identifier? [s]
  (boolean (re-matches (:identifier keywords) s)))

(defn is-number? [s]
  (boolean 
   (or 
    (re-matches (:int_number keywords) s)
    (re-matches (:hex_number keywords) s))))

(defn get-number-type [s]
  (cond 
    (re-matches (:int_number keywords) s) :int_number
    (re-matches (:hex_number keywords) s) :hex_number
    :else :unknown))

(defn is-string? [s]
  (boolean (re-matches (:string keywords) s)))

(defn get-token-type [token]
  (cond
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

(defn token-for-keyword [token keyword-type]
  "Создает токен для ключевого слова с указанным типом"
  {:value token :type keyword-type})

(defn create-token-map []
  "Создает карту токенов для быстрого сопоставления"
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
        :const_keyword :constant}]
    
    (->> (dissoc keywords :identifier :int_number :hex_number :string)
         (mapcat (fn [[k v]]
                   (when (vector? v)
                     (map #(vector % (token-for-keyword % (get keyword-type-mapping k k))) v))))
         (into {})
         ;; Сортируем по длине токена (от большего к меньшему) для корректного совпадения
         (sort-by #(- (count (first %)))))))

(defn find-regex-token [code]
  "Находит токен на основе регулярных выражений"
  (let [hex-match (re-find #"^0[xX][0-9a-fA-F]+" code)]
    (cond 
      ;; Проверяем сначала шестнадцатеричные числа, чтобы не перепутать с идентификаторами
      hex-match
      {:value hex-match
       :type :hex_number}
      
      ;; Проверяем целые числа (без плавающей точки)
      (re-find #"^[-+]?[0-9]+" code)
      {:value (re-find #"^[-+]?[0-9]+" code)
       :type :int_number}
      
      ;; Проверяем строки
      (and (str/starts-with? code "\"") 
           (> (count code) 1)
           (re-find #"^\"[^\"]*\"" code))
      {:value (re-find #"^\"[^\"]*\"" code)
       :type :string}
      
      ;; Проверяем идентификаторы
      (re-find #"^[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*" code)
      {:value (re-find #"^[a-zA-Z_\p{L}][a-zA-Z0-9_\p{L}]*" code)
       :type :identifier}
      
      :else nil)))

(defn tokenize [code]
  "Преобразует исходный код в последовательность токенов"
  (let [token-map (create-token-map)]
    (loop [remaining-code (str/trim code)
           tokens []]
      (if (str/blank? remaining-code)
        tokens
        (let [;; Сначала пытаемся найти точное совпадение токена
              exact-match (some (fn [[token token-info]]
                                  (when (str/starts-with? remaining-code token)
                                    token-info))
                                token-map)
              
              ;; Если точного совпадения нет, пытаемся найти по регулярным выражениям
              token (or exact-match (find-regex-token remaining-code))]
          
          (if token
            (let [value (:value token)
                  token-length (if (string? value) 
                                 (count value) 
                                 ;; Для случая, когда value не строка (возвращаемое значение из re-find может быть вектором)
                                 (count (first (if (vector? value) value [value]))))]
              (recur 
               (str/trim (subs remaining-code token-length))
               (conj tokens (if (vector? (:value token))
                              ;; Если значение - вектор, берем первый элемент (полное совпадение)
                              (assoc token :value (first (:value token)))
                              token))))
            (throw (ex-info "Tokenization error" {:remaining-code remaining-code}))))))))