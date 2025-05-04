(ns c51cc.parser
  "Модуль для синтаксического анализатора C51"
  (:require [c51cc.lexer :as lexer]
            [c51cc.logger :as log]
            [c51cc.ast :as ast]
            [clojure.string :as str]))

;; Предварительные объявления функций
(declare parse-expression
         parse-parameters
         parse-function-body
         parse-statement
         parse-function-call
         parse-assignment
         parse-if-else
         parse-while-loop
         parse-for-loop
         parse-return
         parse-arguments)

;; Типы узлов AST
(def node-types
  "Типы узлов абстрактного синтаксического дерева"
  ast/node-types)

;; Приоритеты операторов
(def operator-precedence
  "Таблица приоритетов операторов"
  {"||" 1, "&&" 1,
   "|"  2, "^"  2, "&" 2,
   "==" 3, "!=" 3,
   "<"  4, ">"  4, "<=" 4, ">=" 4,
   "<<" 5, ">>" 5,
   "+"  6, "-"  6,
   "*"  7, "/"  7, "%" 7,
   "++" 8, "--" 8, "!" 8, "~" 8})

;; =============================================
;; Вспомогательные функции
;; =============================================

(defn peek-token
  "Возвращает следующий токен без его удаления"
  [tokens]
  (first tokens))

(defn expect-token
  "Проверяет, соответствует ли следующий токен ожидаемому типу и значению"
  [tokens expected-type expected-value]
  (let [token (peek-token tokens)]
    (when (or (not= (:type token) expected-type)
              (and expected-value (not= (:value token) expected-value)))
      (throw (ex-info "Unexpected token"
                     {:expected {:type expected-type :value expected-value}
                      :received token})))))

(defn consume-token
  "Потребляет токен и возвращает оставшиеся"
  [tokens]
  (rest tokens))

(defn match-token
  "Проверяет, соответствует ли токен типу и значению, и потребляет его"
  [tokens type value]
  (when (and (= (:type (peek-token tokens)) type)
             (= (:value (peek-token tokens)) value))
    (consume-token tokens)))

;; =============================================
;; Парсинг выражений
;; =============================================

(defn parse-primary-expression
  "Парсинг первичных выражений (идентификаторы, числа, строки, скобки)"
  [tokens]
  (let [token (peek-token tokens)]
    (case (:type token)
      :identifier {:node {:type :identifier
                         :value (:value token)}
                  :tokens (consume-token tokens)}
      
      (:int_number :hex_number)
      {:node {:type :number
              :value (:value token)}
       :tokens (consume-token tokens)}
      
      :string {:node {:type :string
                     :value (:value token)}
              :tokens (consume-token tokens)}
      
      :separator (if (= (:value token) "(")
                  (let [tokens (consume-token tokens)
                        expr (parse-expression tokens)
                        tokens (:tokens expr)]
                    (expect-token tokens :separator ")")
                    {:node (:node expr)
                     :tokens (consume-token tokens)})
                  (throw (ex-info "Expected primary expression"
                                {:token token})))
      
      (throw (ex-info "Expected primary expression"
                     {:token token})))))

(defn parse-unary-expression
  "Парсинг унарных выражений"
  [tokens]
  (let [token (peek-token tokens)]
    (if (and (= (:type token) :operator)
             (contains? #{"!" "~" "++" "--"} (:value token)))
      (let [tokens (consume-token tokens)
            operand (parse-unary-expression tokens)]
        {:node {:type :unary-operation
                :operator (:value token)
                :operand (:node operand)}
         :tokens (:tokens operand)})
      (parse-primary-expression tokens))))

(defn parse-binary-operation
  "Парсинг бинарных операций с учетом приоритета"
  [tokens min-precedence]
  (let [left-expr (parse-unary-expression tokens)
        tokens (:tokens left-expr)]
    (loop [result left-expr
           current-tokens tokens]
      (let [op-token (peek-token current-tokens)]
        (if (and (= (:type op-token) :operator)
                 (>= (get operator-precedence (:value op-token) -1) min-precedence))
          (let [op-precedence (get operator-precedence (:value op-token))
                tokens (consume-token current-tokens)
                right-expr (parse-binary-operation tokens (inc op-precedence))]
            (recur
             {:node {:type :binary-operation
                    :operator (:value op-token)
                    :left (:node result)
                    :right (:node right-expr)}
              :tokens (:tokens right-expr)}
             (:tokens right-expr)))
          result)))))

(defn parse-expression
  "Парсинг выражений"
  [tokens]
  (parse-binary-operation tokens 0))

;; =============================================
;; Парсинг объявлений
;; =============================================

(defn parse-variable-declaration
  "Парсинг объявления переменной"
  [tokens]
  (let [type-token (peek-token tokens)
        tokens (consume-token tokens)
        name-token (peek-token tokens)
        tokens (consume-token tokens)]
    {:node {:type :variable-declaration
            :var-type (:value type-token)
            :name (:value name-token)}
     :tokens tokens}))

(defn parse-function-declaration
  "Парсинг объявления функции"
  [tokens]
  (let [return-type (peek-token tokens)
        tokens (consume-token tokens)
        name-token (peek-token tokens)
        tokens (consume-token tokens)]
    (expect-token tokens :separator "(")
    (let [tokens (consume-token tokens)
          params-result (parse-parameters tokens)
          tokens (:tokens params-result)]
      (expect-token tokens :separator "{")
      (let [tokens (consume-token tokens)
            body-result (parse-function-body tokens)
            tokens (:tokens body-result)]
        {:node {:type :function-declaration
                :return-type (:value return-type)
                :name (:value name-token)
                :parameters (:parameters params-result)
                :body (:body body-result)}
         :tokens tokens}))))

(defn parse-array-declaration
  "Парсинг объявления массива"
  [tokens]
  (let [type-token (peek-token tokens)
        tokens (consume-token tokens)
        name-token (peek-token tokens)
        tokens (consume-token tokens)]
    (expect-token tokens :separator "[")
    (let [tokens (consume-token tokens)
          size-expr (parse-expression tokens)
          tokens (:tokens size-expr)]
      (expect-token tokens :separator "]")
      {:node {:type :array-declaration
              :element-type (:value type-token)
              :name (:value name-token)
              :size (:node size-expr)}
       :tokens (consume-token tokens)})))

(defn parse-pointer-declaration
  "Парсинг объявления указателя"
  [tokens]
  (let [type-token (peek-token tokens)
        tokens (consume-token tokens)
        _ (expect-token tokens :operator "*")
        tokens (consume-token tokens)
        name-token (peek-token tokens)]
    {:node {:type :pointer-declaration
            :pointed-type (:value type-token)
            :name (:value name-token)}
     :tokens (consume-token tokens)}))

(defn parse-parameters
  "Парсинг параметров функции"
  [tokens]
  (loop [current-tokens tokens
         params []]
    (let [token (peek-token current-tokens)]
      (if (or (nil? token)
              (= (:value token) ")"))
        {:parameters params
         :tokens (if token (consume-token current-tokens) current-tokens)}
        (let [type-token token
              tokens (consume-token current-tokens)
              name-token (peek-token tokens)
              tokens (consume-token tokens)
              param {:type (:value type-token)
                    :name (:value name-token)}
              next-token (peek-token tokens)]
          (if (= (:value next-token) ",")
            (recur (consume-token tokens) (conj params param))
            (recur tokens (conj params param))))))))

(defn parse-function-body
  "Парсинг тела функции"
  [tokens]
  (loop [current-tokens tokens
         statements []]
    (let [token (peek-token current-tokens)]
      (if (or (nil? token)
              (= (:value token) "}"))
        {:body statements
         :tokens (if token (consume-token current-tokens) current-tokens)}
        (let [stmt (parse-statement current-tokens)]
          (recur (:tokens stmt)
                 (conj statements (:node stmt))))))))

(defn parse-statement
  "Парсинг оператора"
  [tokens]
  (let [token (peek-token tokens)]
    (case (:type token)
      :type-keyword (parse-variable-declaration tokens)
      :identifier (if (= (:value (second tokens)) "(")
                   (parse-function-call tokens)
                   (parse-assignment tokens))
      :keyword (case (:value token)
                "if" (parse-if-else tokens)
                "while" (parse-while-loop tokens)
                "for" (parse-for-loop tokens)
                "return" (parse-return tokens)
                (throw (ex-info "Unknown keyword" {:token token})))
      (throw (ex-info "Unknown statement type" {:token token})))))

(defn parse-if-else
  "Парсинг условного оператора if-else"
  [tokens]
  (expect-token tokens :keyword "if")
  (let [tokens (consume-token tokens)]
    (expect-token tokens :separator "(")
    (let [tokens (consume-token tokens)
          condition (parse-expression tokens)
          tokens (:tokens condition)]
      (expect-token tokens :separator ")")
      (let [tokens (consume-token tokens)
            then-branch (parse-statement tokens)
            tokens (:tokens then-branch)
            [else-branch tokens] (if (= (:value (peek-token tokens)) "else")
                                 (let [tokens (consume-token tokens)
                                       else-stmt (parse-statement tokens)]
                                   [(:node else-stmt) (:tokens else-stmt)])
                                 [nil tokens])]
        {:node {:type :if-statement
                :condition (:node condition)
                :then-branch (:node then-branch)
                :else-branch else-branch}
         :tokens tokens}))))

(defn parse-while-loop
  "Парсинг цикла while"
  [tokens]
  (expect-token tokens :keyword "while")
  (let [tokens (consume-token tokens)]
    (expect-token tokens :separator "(")
    (let [tokens (consume-token tokens)
          condition (parse-expression tokens)
          tokens (:tokens condition)]
      (expect-token tokens :separator ")")
      (let [tokens (consume-token tokens)
            body (parse-statement tokens)]
        {:node {:type :while-statement
                :condition (:node condition)
                :body (:node body)}
         :tokens (:tokens body)}))))

(defn parse-for-loop
  "Парсинг цикла for"
  [tokens]
  (expect-token tokens :keyword "for")
  (let [tokens (consume-token tokens)]
    (expect-token tokens :separator "(")
    (let [tokens (consume-token tokens)
          init (parse-statement tokens)
          condition (parse-expression (:tokens init))
          tokens (:tokens condition)]
      (expect-token tokens :separator ";")
      (let [tokens (consume-token tokens)
            increment (parse-expression tokens)
            tokens (:tokens increment)]
        (expect-token tokens :separator ")")
        (let [tokens (consume-token tokens)
              body (parse-statement tokens)]
          {:node {:type :for-statement
                  :init (:node init)
                  :condition (:node condition)
                  :increment (:node increment)
                  :body (:node body)}
           :tokens (:tokens body)})))))

(defn parse-return
  "Парсинг оператора return"
  [tokens]
  (expect-token tokens :keyword "return")
  (let [tokens (consume-token tokens)
        expr (parse-expression tokens)
        tokens (:tokens expr)]
    (expect-token tokens :separator ";")
    {:node {:type :return-statement
            :value (:node expr)}
     :tokens (consume-token tokens)}))

(defn parse-function-call
  "Парсинг вызова функции"
  [tokens]
  (let [name-token (peek-token tokens)
        tokens (consume-token tokens)]
    (expect-token tokens :separator "(")
    (let [tokens (consume-token tokens)
          args-result (parse-arguments tokens)
          tokens (:tokens args-result)]
      {:node {:type :function-call
              :name (:value name-token)
              :arguments (:arguments args-result)}
       :tokens tokens})))

(defn parse-arguments
  "Парсинг аргументов функции"
  [tokens]
  (loop [current-tokens tokens
         args []]
    (let [token (peek-token current-tokens)]
      (if (or (nil? token)
              (= (:value token) ")"))
        {:arguments args
         :tokens (if token (consume-token current-tokens) current-tokens)}
        (let [arg (parse-expression current-tokens)
              tokens (:tokens arg)
              next-token (peek-token tokens)]
          (if (= (:value next-token) ",")
            (recur (consume-token tokens) (conj args (:node arg)))
            (recur tokens (conj args (:node arg)))))))))

(defn parse-assignment
  "Парсинг оператора присваивания"
  [tokens]
  (let [target (parse-expression tokens)
        tokens (:tokens target)]
    (expect-token tokens :operator "=")
    (let [tokens (consume-token tokens)
          value (parse-expression tokens)
          tokens (:tokens value)]
      (expect-token tokens :separator ";")
      {:node {:type :assignment
              :target (:node target)
              :value (:node value)}
       :tokens (consume-token tokens)})))

(defn parse
  "Основная функция парсинга"
  [input]
  (let [tokens (if (sequential? input)
                 input
                 (lexer/tokenize input))]
    (loop [current-tokens tokens
           nodes []]
      (if (empty? current-tokens)
        {:type :program
         :nodes nodes}
        (let [node (parse-statement current-tokens)]
          (recur (:tokens node)
                 (conj nodes (:node node))))))))

;; =============================================
;; Парсинг управляющих конструкций
;; =============================================

(defn parse-do-while
  "Парсинг цикла do-while"
  [tokens]
  (expect-token tokens :keyword "do")
  (let [tokens (consume-token tokens)
        body (parse-statement tokens)
        tokens (:tokens body)]
    (expect-token tokens :keyword "while")
    (let [tokens (consume-token tokens)]
      (expect-token tokens :separator "(")
      (let [tokens (consume-token tokens)
            condition (parse-expression tokens)
            tokens (:tokens condition)]
        (expect-token tokens :separator ")")
        (let [tokens (consume-token tokens)]
          (expect-token tokens :separator ";")
          {:node {:type :control-flow
                  :subtype :do-while
                  :body (:node body)
                  :condition (:node condition)}
           :tokens (consume-token tokens)})))))

(defn parse-switch-case
  "Парсинг конструкции switch-case"
  [tokens]
  (expect-token tokens :keyword "switch")
  (let [tokens (consume-token tokens)]
    (expect-token tokens :separator "(")
    (let [tokens (consume-token tokens)
          condition (parse-expression tokens)
          tokens (:tokens condition)]
      (expect-token tokens :separator ")")
      (let [tokens (consume-token tokens)]
        (expect-token tokens :separator "{")
        (loop [current-tokens (consume-token tokens)
               cases []
               default-case nil]
          (let [token (peek-token current-tokens)]
            (cond
              (= (:value token) "}")
              {:node {:type :control-flow
                      :subtype :switch-case
                      :condition (:node condition)
                      :cases cases
                      :default-case default-case}
               :tokens (consume-token current-tokens)}

              (= (:value token) "case")
              (let [tokens (consume-token current-tokens)
                    value (parse-expression tokens)
                    tokens (:tokens value)]
                (expect-token tokens :separator ":")
                (let [tokens (consume-token tokens)
                      body (parse-statement tokens)]
                  (recur (:tokens body)
                         (conj cases {:value (:node value)
                                    :body (:node body)})
                         default-case)))

              (= (:value token) "default")
              (let [tokens (consume-token current-tokens)]
                (expect-token tokens :separator ":")
                (let [tokens (consume-token tokens)
                      body (parse-statement tokens)]
                  (recur (:tokens body)
                         cases
                         (:node body))))

              :else
              (throw (ex-info "Expected case or default"
                            {:token token})))))))))

(defn parse-break-continue
  "Парсинг операторов break и continue"
  [tokens]
  (let [token (peek-token tokens)
        type (case (:value token)
               "break" :break
               "continue" :continue
               (throw (ex-info "Expected break or continue"
                             {:token token})))
        tokens (consume-token tokens)]
    (expect-token tokens :separator ";")
    {:node {:type :control-flow
            :subtype type}
     :tokens (consume-token tokens)})) 