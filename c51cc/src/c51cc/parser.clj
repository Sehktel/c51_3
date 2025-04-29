(ns c51cc.parser
  "Модуль PEG-парсера для языка C51
   
   Теоретические аспекты:
   - Использование Parsing Expression Grammar (PEG)
   - Поддержка синтаксиса языка C
   - Расширенный семантический анализ
   - Гибкая архитектура парсера"
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [c51cc.logger :as log]
            [c51cc.lexer :as lexer]))

;; Определение грамматики PEG для подмножества языка C
(def c-grammar
  "Грамматика для парсинга подмножества языка C"
  "program = function-definition*

  function-definition = type-specifier identifier '(' parameter-list? ')' compound-statement

  parameter-list = 'void' | (parameter (',' parameter)*)?
  parameter = type-specifier identifier?

  type-specifier = 'void' | 'unsigned' | 'char'

  compound-statement = '{' statement* '}'

  statement = variable-declaration
            | assignment-statement
            | ';'

  variable-declaration = type-specifier identifier (',' identifier)* ';'

  assignment-statement = identifier '=' expression ';'

  expression = number
             | identifier
             | expression '+' expression

  identifier = #'[a-zA-Z_][a-zA-Z0-9_]*'
  number = #'[0-9]+'
  ")

;; Создание парсера с использованием instaparse
(def parse-c
  (insta/parser c-grammar 
                :output-format :enlive))

(defn parse
  "Основная функция парсинга с расширенной обработкой ошибок
   
   Параметры:
   - tokens: Последовательность токенов для парсинга
   - opts: Дополнительные опции парсера
   
   Возвращает:
   - Абстрактное синтаксическое дерево (AST)
   
   Сложность: O(n), где n - количество токенов"
  [tokens & [opts]]
  (log/info "Начало парсинга")
  (log/debug "Количество токенов:" (count tokens))
  
  (try
    (let [input (str/join " " (map :value tokens))]
      (log/info "Входная строка для парсинга:" input)
      (log/info "Токены для отладки:")
      (doseq [token tokens]
        (log/info (format "Тип: %-15s Значение: %s" 
                           (str (:type token)) 
                           (:value token))))
      
      (let [parse-result (parse-c input)]
        (log/info "Парсинг успешно завершен")
        (log/debug "Структура AST:" 
                   (with-out-str 
                     (pprint/pprint parse-result)))
        
        {:type :program
         :nodes (if (insta/failure? parse-result)
                  (do 
                    (log/error "Ошибка парсинга:" 
                               (insta/get-failure parse-result))
                    (throw (ex-info "Ошибка парсинга" 
                                    {:failure (insta/get-failure parse-result)})))
                  parse-result)}))
    (catch Exception e
      (log/error "Критическая ошибка при парсинге:" (.getMessage e))
      (throw (ex-info "Ошибка парсинга" 
                      {:tokens tokens
                       :error (.getMessage e)})))))

(defn transform-ast
  "Трансформация AST с семантическим анализом
   
   Параметры:
   - ast: Исходное абстрактное синтаксическое дерево
   
   Возвращает:
   - Трансформированное AST с дополнительными метаданными
   
   Теоретические аспекты:
   - Семантический анализ
   - Обогащение AST метаинформацией
   - Подготовка к генерации кода"
  [ast]
  (log/info "Начало трансформации AST")
  (let [transformed-ast 
        (insta/transform 
         {:program (fn [& nodes] 
                     {:type :program
                      :nodes (vec nodes)
                      :metadata {:total-nodes (count nodes)}})
          
          :function-definition 
          (fn [return-type name params body]
            {:type :function
             :return-type return-type
             :name name
             :parameters params
             :body body})
          
          :declaration 
          (fn [type name & [init-value]]
            {:type :variable-declaration
             :var-type type
             :name name
             :initial-value init-value})}
         ast)]
    (log/debug "Трансформация AST завершена")
    transformed-ast))

(defn analyze-ast
  "Глубокий семантический анализ AST
   
   Параметры:
   - ast: Абстрактное синтаксическое дерево
   
   Возвращает:
   - Результаты семантического анализа
   
   Сложность: O(n), где n - количество узлов в AST"
  [ast]
  (log/info "Начало семантического анализа AST")
  (let [analysis 
        {:node-types (frequencies (map :type (:nodes ast)))
         :total-nodes (count (:nodes ast))
         :function-names (keep #(when (= (:type %) :function) (:name %)) 
                               (:nodes ast))}]
    (log/debug "Семантический анализ завершен")
    analysis))

(defn pretty-print-ast
  "Расширенная визуализация AST
   
   Параметры:
   - ast: Абстрактное синтаксическое дерево
   
   Теоретические аспекты:
   - Многоуровневая визуализация
   - Ограничение глубины для читаемости
   - Семантическая аннотация"
  [ast]
  (with-out-str
    (pprint/pprint 
     (update-in ast [:nodes] 
                (fn [nodes] 
                  (mapv 
                   (fn [node]
                     (let [base-node 
                           (select-keys node 
                                        [:type :name :return-type 
                                         :parameters :body])]
                       (cond-> base-node
                         (:parameters base-node) 
                         (update :parameters 
                                 (fn [params] 
                                   (take 10 params)))
                         
                         (:body base-node) 
                         (update :body 
                                 (fn [body] 
                                   (take 100 body))))))
                   (take 100 nodes))))))

;; Пример использования парсера
(defn example-parse 
  "Демонстрационный пример парсинга простой функции"
  []
  (let [tokens [{:type :type-keyword :value "int"}
                {:type :identifier :value "main"}
                {:type :separator :value "("}
                {:type :separator :value ")"}
                {:type :separator :value "{"}
                {:type :type-keyword :value "int"}
                {:type :identifier :value "x"}
                {:type :operator :value "="}
                {:type :int_number :value "10"}
                {:type :separator :value ";"}
                {:type :return-keyword :value "return"}
                {:type :int_number :value "0"}
                {:type :separator :value ";"}
                {:type :separator :value "}"}]
        ast (parse tokens)
        transformed-ast (transform-ast (:nodes ast))
        analysis (analyze-ast {:nodes transformed-ast})]
    (println "=== AST ===")
    (pprint/pprint ast)
    (println "\n=== Transformed AST ===")
    (pprint/pprint transformed-ast)
    (println "\n=== AST Analysis ===")
    (pprint/pprint analysis)
    analysis)))

(defn parse-file
  "Полный цикл парсинга файла: чтение, токенизация, парсинг и анализ
   
   Параметры:
   - file-path: Путь к файлу для парсинга
   
   Возвращает:
   - Полный результат парсинга с AST и анализом"
  [file-path]
  (log/info (str "Начало парсинга файла: " file-path))
  
  (try 
    ;; Чтение содержимого файла
    (let [file-content (slurp file-path)
          
          ;; Токенизация с использованием лексера
          tokens (lexer/tokenize file-content)
          
          ;; Парсинг токенов
          ast (parse tokens)
          
          ;; Трансформация AST
          transformed-ast (transform-ast (:nodes ast))
          
          ;; Анализ AST
          analysis (analyze-ast {:nodes transformed-ast})]
      
      (log/info "Парсинг файла успешно завершен")
      
      ;; Вывод результатов
      (println "=== Токены ===")
      (pprint/pprint tokens)
      
      (println "\n=== AST ===")
      (pprint/pprint ast)
      
      (println "\n=== Трансформированный AST ===")
      (pprint/pprint transformed-ast)
      
      (println "\n=== Анализ AST ===")
      (pprint/pprint analysis)
      
      ;; Возврат полного результата
      {:file-path file-path
       :tokens tokens
       :ast ast
       :transformed-ast transformed-ast
       :analysis analysis})
    
    (catch Exception e
      (println "Ошибка при парсинге:")
      (println "Сообщение:" (.getMessage e))
      (println "Трассировка стека:")
      (.printStackTrace e)
      nil)))

(defn parse-abc-example 
  "Демонстрация парсинга примера abc.c с подробным анализом"
  []
  (try 
    (let [file-path "test/c51code/abc.c"
          file-content (slurp file-path)
          
          ;; Токенизация с использованием лексера
          tokens (lexer/tokenize file-content)
          
          ;; Парсинг токенов
          ast (parse tokens)
          
          ;; Трансформация AST
          transformed-ast (transform-ast (:nodes ast))
          
          ;; Анализ AST
          analysis (analyze-ast {:nodes transformed-ast})]
      
      ;; Подробный вывод
      (println "=== Исходный код ===")
      (println file-content)
      
      (println "\n=== Токены ===")
      (doseq [token tokens]
        (println (format "Тип: %-15s Значение: %s" 
                         (str (:type token)) 
                         (:value token))))
      
      (println "\n=== Абстрактное Синтаксическое Дерево (AST) ===")
      (pprint/pprint ast)
      
      (println "\n=== Трансформированный AST ===")
      (pprint/pprint transformed-ast)
      
      (println "\n=== Анализ AST ===")
      (pprint/pprint analysis)
      
      ;; Возврат результатов для дальнейшего использования
      {:file-path file-path
       :tokens tokens
       :ast ast
       :transformed-ast transformed-ast
       :analysis analysis})
    (catch Exception e
      (println "Ошибка при парсинге:")
      (println "Сообщение:" (.getMessage e))
      (println "Трассировка стека:")
      (.printStackTrace e)
      nil)))

(defn -main 
  "Точка входа для запуска парсера"
  [& args]
  (println "Запуск парсера для файла:" (first args))
  (parse-file (first args)))