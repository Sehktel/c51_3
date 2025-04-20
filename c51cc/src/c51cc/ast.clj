(ns c51cc.ast
  "Модуль для визуализации и печати абстрактного синтаксического дерева (AST)"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [c51cc.logger :as log]
            [clojure.pprint :as pprint]))

(defn- indent-str
  "Создает строку отступа для визуализации иерархии AST"
  [depth]
  (apply str (repeat (* depth 2) " ")))

(defn print-ast-node
  "Рекурсивная функция для печати узла AST с учетом его структуры

  Теоретическое обоснование:
  - Глубокая рекурсивная визуализация структуры AST
  - Поддержка различных типов узлов
  - Иерархическое представление с отступами

  Сложность: O(n), где n - количество узлов в дереве"
  ([node] (print-ast-node node 0))
  ([node depth]
   (let [indent (indent-str depth)]
     (log/debug (str indent "Тип узла: " (:type node)))
     (case (:type node)
       :program 
       (do 
         (log/debug (str indent "Количество узлов: " (count (:nodes node))))
         (doseq [child-node (:nodes node)]
           (print-ast-node child-node (inc depth))))
       
       :function-declaration
       (do
         (log/debug (str indent "Имя функции: " (:name node)))
         (log/debug (str indent "Тип возвращаемого значения: " (:return-type node)))
         (log/debug (str indent "Параметры:"))
         (doseq [param (:parameters node)]
           (log/debug (str (indent-str (inc depth)) 
                           "Тип: " (:type param) 
                           " Имя: " (:name param))))
         (log/debug (str indent "Тело функции:"))
         (doseq [body-token (:body node)]
           (log/debug (str (indent-str (inc depth)) (pr-str body-token)))))
       
       :variable-declaration
       (do
         (log/debug (str indent "Имя переменной: " (:name node)))
         (log/debug (str indent "Тип переменной: " (:var-type node))))
       
       :expression
       (log/debug (str indent "Значение выражения: " (:value node)))
       
       ;; Обработка неизвестных типов узлов
       (log/debug (str indent "Неизвестный тип узла: " (:type node)))))))

(defn print-ast
  "Главная функция для печати AST дерева

  Архитектурные соображения:
  - Абстракция над парсингом и визуализацией
  - Гибкая обработка различных входных данных

  @param input - может быть строкой исходного кода или последовательностью токенов"
  [input]
  (log/debug "Начало визуализации Abstract Syntax Tree")
  (let [tokens (if (string? input)
                 (lexer/tokenize input)
                 input)
        ast (parser/parse tokens)]
    (log/info "=== Визуализация Abstract Syntax Tree ===")
    (print-ast-node ast)
    (log/info "=========================================")))

(defn pretty-print-ast
  "Улучшенная функция печати AST с использованием pretty-print

  Преимущества:
  - Более читаемый вывод
  - Поддержка сложных структур данных"
  [input]
  (log/debug "Начало детальной визуализации AST")
  (let [tokens (if (string? input)
                 (lexer/tokenize input)
                 input)
        ast (parser/parse tokens)]
    (log/info "=== Детальная визуализация AST ===")
    (pprint/pprint ast)
    (log/info "==================================")))