(ns c51cc.parser
  "Модуль PEG-парсера для языка C51"
  (:require [instaparse.core :as insta]
            [c51cc.logger :as log]
            [c51cc.lexer :as lexer]
            [c51cc.grammar :as grammar]
            [clojure.string :as str]))

(defn create-parser 
  "Создание PEG-парсера с расширенной диагностикой"
  []
  (log/debug "Создание PEG-парсера с расширенной диагностикой")
  (insta/parser grammar/c51-grammar 
                :output-format :enlive
                :trace true))

(defn tokenize-input
  "Токенизация входных данных с использованием лексера"
  [input]
  (log/debug "Токенизация входных данных:" input)
  (let [tokens (lexer/tokenize input)]
    (log/debug "Полученные токены:" tokens)
    tokens))

(declare transform-ast)

(defn parse 
  "Основная функция парсинга с расширенной обработкой ошибок"
  [input]
  (log/debug "Начало PEG-парсинга для входных данных:" input)
  (let [parser (create-parser)
        tokens (tokenize-input input)]
    (try 
      (let [result (parser input)]
        (if (insta/failure? result)
          (do 
            (log/error "Ошибка парсинга:" (insta/get-failure result))
            {:error (insta/get-failure result)})
          (let [semantic-ast (transform-ast result)]
            (log/debug "Успешный парсинг и трансформация AST:" semantic-ast)
            {:ast semantic-ast
             :tokens tokens})))
      (catch Exception e
        (log/error "Критическая ошибка парсинга:" (.getMessage e))
        {:error (.getMessage e)
         :tokens tokens}))))

;; Вспомогательные функции для трансформации
(defn- extract-type 
  "Извлечение типа из узла AST"
  [type-node]
  (first (get-in type-node [:content])))

(defn- extract-identifier 
  "Извлечение идентификатора из узла AST"
  [identifier-node]
  (first (get-in identifier-node [:content])))

(defn- extract-interrupt-number
  "Извлечение номера прерывания из узла AST с максимально надежным подходом"
  [interrupt-node]
  (log/debug "Полное содержимое узла прерывания:" (pr-str interrupt-node))
  (let [content (:content interrupt-node)
        _ (log/debug "Содержимое узла:" (pr-str content))]
    (try 
      (let [num-candidates (filter 
                            #(or 
                              (= (:tag %) :number)
                              (= (:tag %) :integer-number)
                              (= (:tag %) :int_number)) 
                            content)]
        (log/debug "Кандидаты на номер прерывания:" (pr-str num-candidates))
        (let [result (cond 
                       (empty? num-candidates) nil
                       (map? (first num-candidates)) 
                       (first (:content (first num-candidates)))
                       (vector? (first num-candidates))
                       (first (first num-candidates))
                       :else 
                       (first num-candidates))]
          (log/debug "Извлеченный номер прерывания:" (pr-str result))
          (cond
            (nil? result) nil
            (string? result) result
            (coll? result) (first result)
            :else (str result))))
      (catch Exception e
        (log/error "Ошибка при извлечении номера прерывания:" e)
        nil))))

(defn- transform-function-declaration 
  "Трансформация узла объявления функции"
  [node]
  (log/debug "Трансформация объявления функции. Входной узел:" (pr-str node))
  (let [children (:content node)
        _ (log/debug "Дочерние узлы:" (pr-str children))
        type-node (first children)
        name-node (second children)
        rest (drop 2 children)
        _ (log/debug "Оставшиеся узлы:" (pr-str rest))
        [params-node rest1] (if (and (seq rest) (= (:tag (first rest)) :parameters))
                              [(first rest) (drop 1 rest)]
                              [nil rest])
        _ (log/debug "Параметры:" (pr-str params-node))
        _ (log/debug "Оставшиеся узлы после параметров:" (pr-str rest1))
        [interrupt-node rest2] (if (and (seq rest1) (= (:tag (first rest1)) :interrupt-specifier))
                                 [(first rest1) (drop 1 rest1)]
                                 [nil rest1])
        _ (log/debug "Узел прерывания:" (pr-str interrupt-node))
        body-node (first rest2)
        params (mapv (fn [param-node]
                       (let [[param-type param-name] (:content param-node)]
                         {:type (extract-type param-type)
                          :name (extract-identifier param-name)}))
                     (if params-node (:content params-node) []))
        interrupt-number (when interrupt-node
                           (extract-interrupt-number interrupt-node))
        result {:type :function
                :return-type (extract-type type-node)
                :name (extract-identifier name-node)
                :parameters params
                :interrupt-number interrupt-number
                :body (get-in body-node [:content])}]
    (log/debug "Результат трансформации функции:" (pr-str result))
    result))

(defn- transform-variable-declaration 
  "Трансформация узла объявления переменной с поддержкой множественных деклараций"
  [node]
  (log/debug "Трансформация объявления переменной. Входной узел:" node)
  (let [[type-node vars-node] (:content node)
        type-str (first (get-in type-node [:content]))
        var-nodes (get-in vars-node [:content] [])
        result {:type :variable
                :var-type type-str
                :variables (filterv (comp not nil? :name) 
                                    (mapv (fn [var-node]
                                            (let [[name-node & init-node] (:content var-node)
                                                  name-str (first (get-in name-node [:content]))]
                                              {:name name-str
                                               :initial-value (when (seq init-node)
                                                               (first (first init-node)))}))
                                          var-nodes))}]
    (log/debug "Результат трансформации переменной:" result)
    result))

(defn- transform-interrupt-declaration 
  "Трансформация узла объявления прерывания"
  [node]
  (log/debug "Трансформация объявления прерывания. Входной узел:" node)
  (let [[interrupt-num-node type-node name-node body-node] (:content node)
        result {:type :interrupt
                :interrupt-number (first (get-in interrupt-num-node [:content]))
                :return-type (extract-type type-node)
                :name (extract-identifier name-node)
                :body (get-in body-node [:content])}]
    (log/debug "Результат трансформации прерывания:" result)
    result))

;; Трансформация AST для дальнейшего анализа
(defn transform-ast 
  "Преобразование PEG AST в более удобную структуру"
  [ast]
  (log/debug "Начало трансформации AST. Входной AST:" ast)
  (let [transform-node (fn [node]
                         (log/trace "Трансформация узла:" node)
                         (cond
                           (and (map? node) (= (:tag node) :declaration))
                           (let [decl-node (first (:content node))]
                             (cond 
                               (= (:tag decl-node) :function-declaration)
                               (transform-function-declaration decl-node)
                               
                               (= (:tag decl-node) :variable-declaration)
                               (transform-variable-declaration decl-node)
                               
                               (= (:tag decl-node) :interrupt-declaration)
                               (transform-interrupt-declaration decl-node)))
                           
                           :else node))
        result (mapv transform-node (:content ast))]
    (log/debug "Результат трансформации AST:" result)
    result))

;; Публичный API для парсинга с трансформацией
(defn parse-with-transform 
  "Полный цикл парсинга с трансформацией AST"
  [input]
  (log/debug "Начало парсинга с трансформацией для входных данных:" input)
  (let [parse-result (parse input)]
    (if (:error parse-result)
      (do 
        (log/error "Ошибка при парсинге:" parse-result)
        parse-result)
      (let [transformed-result (update parse-result :ast transform-ast)]
        (log/debug "Результат парсинга с трансформацией:" transformed-result)
        transformed-result))))