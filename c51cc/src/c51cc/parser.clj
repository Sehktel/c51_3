(ns c51cc.parser
  "Модуль PEG-парсера для языка C51"
  (:require [instaparse.core :as insta]
            [c51cc.logger :as log]
            [clojure.string :as str]))

;; Расширенная PEG-грамматика для языка C51 с максимальной гибкостью
(def c51-grammar
  "program = <whitespace*> (declaration <whitespace*>)* <whitespace*>
   
   declaration = function-declaration / variable-declaration
   
   function-declaration = type-keyword <whitespace+> identifier <whitespace*> 
                          <'('> <whitespace*> parameters? <whitespace*> <')'> 
                          <whitespace*> function-body
   
   function-body = <'{'> <whitespace*> statement* <whitespace*> <'}'>
   
   parameters = parameter (<whitespace*> ',' <whitespace*> parameter)*
   parameter = type-keyword <whitespace+> identifier
   
   variable-declaration = type-keyword <whitespace+> variable-list <';'>
   
   variable-list = variable-init (<whitespace*> ',' <whitespace*> variable-init)*
   variable-init = identifier (<whitespace*> '=' <whitespace*> expression)?
   
   statement = variable-declaration 
             / assignment-statement
             / function-call-statement
             / control-flow 
             / return-statement
             / <whitespace*>
   
   assignment-statement = identifier <whitespace*> '=' <whitespace*> expression <';'>
   function-call-statement = function-call <';'>
   
   return-statement = 'return' <whitespace+> expression <whitespace*> ';'
   
   function-call = identifier <whitespace*> <'('> <whitespace*> arguments? <whitespace*> <')'>
   arguments = expression (<whitespace*> ',' <whitespace*> expression)*
   
   control-flow = ('if' / 'while') <whitespace*> <'('> <whitespace*> expression <whitespace*> <')'> 
                 <whitespace*> <'{'> <whitespace*> statement* <whitespace*> <'}'>
   
   expression = arithmetic-expression 
              / parenthesized-expression
              / identifier 
              / number
   
   parenthesized-expression = <'('> <whitespace*> expression <whitespace*> <')'>
   
   arithmetic-expression = expression <whitespace*> operator <whitespace*> expression
   
   operator = '+' / '-' / '*' / '/' / '==' / '!=' / '<' / '>' / '='
   
   type-keyword = 'int' / 'void' / 'char' / 'void*' / 'unsigned int'
   identifier = #'[a-zA-Z_][a-zA-Z0-9_]*'
   number = #'[0-9]+'
   
   whitespace = #'\\s+'")

;; Создание PEG-парсера с расширенной обработкой ошибок
(defn create-parser 
  "Создание PEG-парсера с расширенной диагностикой"
  []
  (insta/parser c51-grammar 
                :output-format :enlive
                :trace true))

(defn parse 
  "Основная функция парсинга с расширенной обработкой ошибок"
  [input]
  (log/debug "Начало PEG-парсинга")
  (let [parser (create-parser)]
    (try 
      (let [result (parser input)]
        (log/debug "Результат парсинга:" result)
        (if (insta/failure? result)
          (do 
            (log/error "Ошибка парсинга:  " (insta/get-failure result))
            {:error (insta/get-failure result)})
          {:ast result}))
      (catch Exception e
        (log/error "Критическая ошибка парсинга" e)
        {:error (.getMessage e)}))))

;; Вспомогательные функции для трансформации
(defn- extract-type 
  "Извлечение типа из узла AST"
  [type-node]
  (first (get-in type-node [:content])))

(defn- extract-identifier 
  "Извлечение идентификатора из узла AST"
  [identifier-node]
  (first (get-in identifier-node [:content])))

(defn- transform-function-declaration 
  "Трансформация узла объявления функции"
  [node]
  (let [[type-node name-node params-node body-node] (:content node)]
    {:type :function
     :return-type (extract-type type-node)
     :name (extract-identifier name-node)
     :parameters (mapv (fn [param-node]
                         (let [[param-type param-name] (:content param-node)]
                           {:type (extract-type param-type)
                            :name (extract-identifier param-name)}))
                       (get-in params-node [:content] []))
     :body (get-in body-node [:content])}))

(defn- transform-variable-declaration 
  "Трансформация узла объявления переменной с поддержкой множественных деклараций"
  [node]
  (let [[type-node vars-node] (:content node)
        type-str (first (get-in type-node [:content]))
        var-nodes (get-in vars-node [:content] [])]
    {:type :variable
     :var-type type-str
     :variables (filterv (comp not nil? :name) 
                         (mapv (fn [var-node]
                                 (let [[name-node & init-node] (:content var-node)
                                       name-str (first (get-in name-node [:content]))]
                                   {:name name-str
                                    :initial-value (when (seq init-node)
                                                    (first (first init-node)))}))
                               var-nodes))}))

;; Трансформация AST для дальнейшего анализа
(defn transform-ast 
  "Преобразование PEG AST в более удобную структуру"
  [ast]
  (let [transform-node (fn [node]
                         (cond
                           (and (map? node) (= (:tag node) :declaration))
                           (let [decl-node (first (:content node))]
                             (cond 
                               (= (:tag decl-node) :function-declaration)
                               (transform-function-declaration decl-node)
                               
                               (= (:tag decl-node) :variable-declaration)
                               (transform-variable-declaration decl-node)))
                           
                           :else node))]
    (mapv transform-node (:content ast))))

;; Публичный API для парсинга с трансформацией
(defn parse-with-transform 
  "Полный цикл парсинга с трансформацией AST"
  [input]
  (let [parse-result (parse input)]
    (if (:error parse-result)
      parse-result
      (update parse-result :ast transform-ast))))