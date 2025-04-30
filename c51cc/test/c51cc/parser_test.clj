(ns c51cc.parser_test
  "Тесты для синтаксического анализатора"
  (:require [clojure.test :refer :all]
            [c51cc.logger :as log]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn fixture-setup [f]
  (log/set-debug-level! :DEBUG)
  (f))

(use-fixtures :each fixture-setup)

;; Вспомогательная функция для создания токенов с логированием
(defn create-tokens
  "Создание списка токенов для тестирования с отладочным логированием"
  [& token-specs]
  (log/debug "Создание токенов:" token-specs)
  (let [tokens (mapv (fn [[type value]]
                       {:type type :value value})
                     token-specs)]
    (log/debug "Созданные токены:" tokens)
    tokens))

;; Тесты для парсинга программы
(deftest test-parse-program
  (testing "Парсинг пустой программы"
    (let [tokens []
          result (parser/parse-program tokens)]
      (is (= (:type result) :program))
      (is (empty? (:nodes result)))))

  (testing "Парсинг простой программы с пустой функцией"
    (try
      (let [tokens (create-tokens
                    [:type-keyword "void"]
                    [:identifier "main"]
                    [:separator "("]
                    [:separator ")"]
                    [:separator "{"]
                    [:separator "}"])
            _ (log/debug "Созданы токены для теста")
            result (do
                    (log/debug "Начало парсинга программы")
                    (let [r (parser/parse-program tokens)]
                      (log/debug "Парсинг завершен")
                      r))]
        (is (= (:type result) :program))
        (is (= (count (:nodes result)) 1))
        (let [main-func (first (:nodes result))]
          (is (= (:type main-func) :function-declaration))
          (is (= (get-in main-func [:name :value]) "main"))))
      (catch Exception e
        (log/error "Ошибка в тесте:" (str e))
        (throw e)))))

;; Тесты для парсинга функций
(deftest test-parse-function-declaration
  (testing "Корректное объявление функции без параметров"
    ;; (log/set-debug-level! :TRACE)
    (log/debug "Начало теста: Корректное объявление функции")
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "testFunction"]
                  [:separator "("]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator "}"])
          result (parser/parse-function-declaration tokens)]
      (log/debug "Результат парсинга функции:" result)
      (is (= (:type result) :function-declaration))
      (is (= (:return-type result) "int"))
      (is (= (:name result) "testFunction"))
      (is (empty? (:parameters result)))
      (is (= (:body result) []))
      (log/debug "Тест завершен успешно")))

  (testing "Корректное объявление функции с параметрами"
    (log/debug "Начало теста: Функция с параметрами")
    (let [tokens (create-tokens
                  [:type-keyword "void"]
                  [:identifier "functionWithParams"]
                  [:separator "("]
                  [:type-keyword "int"]
                  [:identifier "x"]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator "}"])
          result (parser/parse-function-declaration tokens)]
      (log/debug "Результат парсинга функции:" result)
      (is (= (:type result) :function-declaration))
      (is (= (:return-type result) "void"))
      (is (= (:name result) "functionWithParams"))
      (is (= (count (:parameters result)) 1))
      (is (= (first (:parameters result)) 
             {:type "int" :name "x"}))
      (is (= (count (:body result)) 0))      
      (log/debug "Тест завершен успешно")))

  (testing "Некорректное объявление функции"
    (log/debug "Начало теста: Некорректное объявление функции")
    (let [tokens (create-tokens
                  [:identifier "invalidFunction"]
                  [:separator "("])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Некорректное начало объявления функции"
           (parser/parse-function-declaration tokens)))
      (log/debug "Тест на некорректное объявление функции завершен"))))

;; Тесты для парсинга переменных
(deftest test-parse-variable-declaration
  ;; (testing "Корректное объявление переменной"
  ;;   (log/debug "Начало теста: Корректное объявление переменной")
  ;;   (let [tokens (create-tokens
  ;;                 [:type-keyword "int"]
  ;;                 [:identifier "x"])
  ;;         parser (parser/create-parser tokens)
  ;;         result (parser/parse-variable-declaration tokens)]
  ;;     (log/debug "Результат парсинга переменной:" result)
  ;;     (is (= (:type result) :variable-declaration))
  ;;     (is (= (:var-type result) "int"))
  ;;     (is (= (:name result) "x"))
  ;;     (log/debug "Тест завершен успешно")))

  ;; (testing "Некорректное объявление переменной"
  ;;   (log/debug "Начало теста: Некорректное объявление переменной")
  ;;   (let [tokens (create-tokens
  ;;                 [:identifier "y"])
  ;;         parser (parser/create-parser tokens)]
  ;;     (is (thrown-with-msg?
  ;;          clojure.lang.ExceptionInfo
  ;;          #"Некорректное объявление переменной"
  ;;          (parser/parse-variable-declaration tokens)))
  ;;     (log/debug "Тест на некорректное объявление переменной завершен")))
      )

;; Тесты для парсинга выражений
(deftest test-parse-expression
  ;; (testing "Парсинг идентификатора"
  ;;   (log/debug "Начало теста: Парсинг идентификатора")
  ;;   (let [tokens (create-tokens
  ;;                 [:identifier "variable"])
  ;;         parser (parser/create-parser tokens)
  ;;         result (parser/parse-expression tokens)]
  ;;     (log/debug "Результат парсинга идентификатора:" result)
  ;;     (is (= (:type result) :expression))
  ;;     (is (= (:value result) "variable"))
  ;;     (log/debug "Тест завершен успешно")))

  ;; (testing "Парсинг числа"
  ;;   (log/debug "Начало теста: Парсинг числа")
  ;;   (let [tokens (create-tokens
  ;;                 [:int_number "42"])
  ;;         parser (parser/create-parser tokens)
  ;;         result (parser/parse-expression tokens)]
  ;;     (log/debug "Результат парсинга числа:" result)
  ;;     (is (= (:type result) :expression))
  ;;     (is (= (:value result) "42"))
  ;;     (log/debug "Тест завершен успешно")))

  ;; (testing "Неподдерживаемое выражение"
  ;;   (log/debug "Начало теста: Неподдерживаемое выражение")
  ;;   (let [tokens (create-tokens
  ;;                 [:unknown "something"])
  ;;         parser (parser/create-parser tokens)]
  ;;     (is (thrown-with-msg?
  ;;          clojure.lang.ExceptionInfo
  ;;          #"Неподдерживаемое выражение"
  ;;          (parser/parse-expression tokens)))
  ;;     (log/debug "Тест на неподдерживаемое выражение завершен")))
      )

;; Тесты для парсинга циклов for
(deftest test-parse-for-loop
  (testing "Корректный цикл for с инкрементом"
    (log/debug "Начало теста: Корректный цикл for с инкрементом")
    ;; (log/set-debug-level! :TRACE)
    (let [tokens (create-tokens
                  [:keyword "for"]
                  [:separator "("]
                  [:identifier "i"]
                  [:operator "="]
                  [:int_number "0"]
                  [:separator ";"]
                  [:identifier "i"]
                  [:operator "<"]
                  [:int_number "10"]
                  [:separator ";"]
                  [:identifier "i"]
                  [:operator "++"]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator ";"]
                  [:separator "}"])
          result (parser/parse-for-loop tokens)]
      (log/trace "type result :control-flow:")
      (is (= (:type result) :control-flow))
      (log/trace "subtype result :for-loop:")
      (is (= (:subtype result) :for-loop))
      (log/trace "initialization result :map?")
      (is (map? (:initialization result)))
      (log/trace "condition result :map?")
      (is (map? (:condition result)))
      (log/trace "step result :map?")
      (is (map? (:step result)))
      ;; (log/set-debug-level! :DEBUG)  
      (log/debug "Тест завершен успешно")))

  (testing "Корректный цикл for с декрементом"
    (log/debug "Начало теста: Корректный цикл for с декрементом")
    (let [tokens (create-tokens
                  [:keyword "for"]
                  [:separator "("]
                  [:identifier "i"]
                  [:operator "="]
                  [:int_number "10"]
                  [:separator ";"]
                  [:identifier "i"]
                  [:operator ">"]
                  [:int_number "0"]
                  [:separator ";"]
                  [:identifier "i"]
                  [:operator "--"]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator ";"]
                  [:separator "}"])
          result (parser/parse-for-loop tokens)]
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :for-loop))
      (is (map? (:initialization result)))
      (is (map? (:condition result)))
      (is (map? (:step result)))
      (log/debug "Тест завершен успешно")))

  (testing "Корректный цикл for с составным шагом"
    (log/debug "Начало теста: Корректный цикл for с составным шагом")
    (let [tokens (create-tokens
                  [:keyword "for"]
                  [:separator "("]
                  [:identifier "i"]
                  [:operator "="]
                  [:int_number "0"]
                  [:separator ";"]
                  [:identifier "i"]
                  [:operator "<"]
                  [:int_number "100"]
                  [:separator ";"]
                  [:identifier "i"]
                  [:operator "+="]
                  [:int_number "2"]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator ";"]
                  [:separator "}"])
          result (parser/parse-for-loop tokens)]
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :for-loop))
      (is (map? (:initialization result)))
      (is (map? (:condition result)))
      (is (map? (:step result)))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга циклов while
(deftest test-parse-while-loop
  (testing "Корректный цикл while"
    (log/debug "Начало теста: Корректный цикл while")
    (let [tokens (create-tokens
                  [:keyword "while"]
                  [:separator "("]
                  [:identifier "x"]
                  [:operator ">"]
                  [:int_number "0"]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator "}"])
          ;; parser (parser/create-parser tokens)
          result (parser/parse-while-loop tokens)]
      (log/debug "Результат парсинга while-цикла:" result)
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :while-loop))
      (is (map? (:condition result)))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга switch-case
(deftest test-parse-switch-case
  (testing "Корректная конструкция switch-case"
    (log/debug "Начало теста: Корректный switch-case")
    (let [tokens (create-tokens
                  [:keyword "switch"]
                  [:separator "("]
                  [:identifier "x"]
                  [:separator ")"]
                  [:separator "{"]
                  [:keyword "case"]
                  [:int_number "1"]
                  [:separator ":"]
                  [:keyword "break"]
                  [:separator ";"]
                  [:keyword "default"]
                  [:separator ":"]
                  [:keyword "break"]
                  [:separator ";"]
                  [:separator "}"])
          ;; parser (parser/create-parser tokens)
          result (parser/parse-switch-case tokens)]
      (log/debug "Результат парсинга switch-case:" result)
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :switch-case))
      (is (vector? (:cases result)))
      (is (map? (:default-case result)))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга do-while
(deftest test-parse-do-while
  (testing "Корректный цикл do-while"
    (log/debug "Начало теста: Корректный do-while")
    (let [tokens (create-tokens
                  [:keyword "do"]
                  [:separator "{"]
                  [:separator "}"]
                  [:keyword "while"]
                  [:separator "("]
                  [:identifier "x"]
                  [:operator ">"]
                  [:int_number "0"]
                  [:separator ")"]
                  [:separator ";"])
          result (parser/parse-do-while tokens)]
      (log/debug "Результат парсинга do-while:" result)
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :do-while))
      (is (map? (:condition result)))
      (log/debug "Тест завершен успешно")))
     )

;; Тесты для парсинга if-else
(deftest test-parse-if-else
  (testing "Корректная конструкция if-else"
    (log/debug "Начало теста: Корректный if-else")
    (let [tokens (create-tokens
                  [:keyword "if"]
                  [:separator "("]
                  [:identifier "x"]
                  [:operator "=="]
                  [:int_number "0"]
                  [:separator ")"]
                  [:separator "{"]
                  [:separator ";"]
                  [:separator "}"]
                  [:keyword "else"]
                  [:separator "{"]
                  [:separator ";"]
                  [:separator "}"])
          
          result (parser/parse-if-else tokens)]
      (log/debug "Результат парсинга if-else:" result)
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :if-else))
      (is (map? (:condition result)))
      (is (vector? (:true-block result)))
      (is (vector? (:false-block result)))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга break/continue
(deftest test-parse-break-continue
  (testing "Корректный оператор break"
    (log/debug "Начало теста: Корректный break")
    (let [tokens (create-tokens
                  [:keyword "break"]
                  [:separator ";"])
          
          result (parser/parse-break-continue tokens)]
      (log/debug "Результат парсинга break:" result)
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :break))
      (log/debug "Тест завершен успешно")))

  (testing "Корректный оператор continue"
    (let [tokens (create-tokens
                  [:keyword "continue"]
                  [:separator ";"])
          
          result (parser/parse-break-continue tokens)]
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :continue)))))

;; Тесты для парсинга return
(deftest test-parse-return
  (testing "Корректный оператор return с выражением"
    (log/debug "Начало теста: Корректный return")
    (let [tokens (create-tokens
                  [:keyword "return"]
                  [:identifier "x"]
                  [:separator ";"])
          
          result (parser/parse-return  tokens)]
      (log/debug "Результат парсинга return:" result)
      (is (= (:type result) :control-flow))
      (is (= (:subtype result) :return))
      (is (map? (:value result)))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга указателей
(deftest test-parse-pointer-declaration
  (testing "Корректное объявление указателя"
    (log/debug "Начало теста: Корректное объявление указателя")
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:operator "*"]
                  [:identifier "ptr"]
                  [:separator ";"])
          
          result (parser/parse-pointer-declaration tokens)]
      (log/debug "Результат парсинга указателя:" result)
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "int*"))
      (is (:is-pointer result))
      (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга массивов
(deftest test-parse-array-declaration
  (testing "Корректное объявление массива"
    (log/debug "Начало теста: Корректное объявление массива")
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "foo"]
                  [:separator "["]
                  [:int_number "10"]
                  [:separator "]"]
                  [:separator ";"])
          
          result (parser/parse-array-declaration tokens)]
      (log/debug "Результат парсинга массива:" result)
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "int[]"))
      (is (:is-array result))
      (is (map? (:array-size result)))
      (log/debug "Тест завершен успешно"))))

(deftest test-parse-array-initialization
  (testing "Объявление массива с инициализацией простыми значениями"
    (log/debug "Начало теста: Массив с инициализацией простыми значениями")
    (let [tokens (create-tokens
                  [:type-keyword "int"]
                  [:identifier "numbers"]
                  [:separator "["]
                  [:int_number "5"]
                  [:separator "]"]
                  [:operator "="]
                  [:separator "{"]
                  [:int_number "1"]
                  [:separator ","]
                  [:int_number "2"]
                  [:separator ","]
                  [:int_number "3"]
                  [:separator ","]
                  [:int_number "4"]
                  [:separator ","]
                  [:int_number "5"]
                  [:separator "}"]
                  [:separator ";"])
          
          result (parser/parse-array-declaration tokens)]
      (log/debug "Результат парсинга массива с инициализацией:" result)
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "int[]"))
      (is (:is-array result))
      (is (map? (:array-size result)))
      (is (= (get-in (:array-size result) [:value]) "5"))
      (is (= (:type (:init-values result)) :array-initializer))
      (is (= (count (:values (:init-values result))) 5))
      (log/debug "Тест завершен успешно"))))

(deftest test-parse-array-mixed-initialization
  (testing "Объявление массива с инициализацией переменными"
    (log/debug "Начало теста: Массив с инициализацией переменными")
    (let [tokens (create-tokens
                  [:type-keyword "unsigned"]
                  [:type-keyword "char"]
                  [:identifier "data"]
                  [:separator "["]
                  [:int_number "4"]
                  [:separator "]"]
                  [:operator "="]
                  [:separator "{"]
                  [:int_number "10"]
                  [:separator ","]
                  [:int_number "20"]
                  [:separator ","]
                  [:int_number "30"]
                  [:separator ","]
                  [:int_number "40"]
                  [:separator "}"]
                  [:separator ";"])
          
          result (parser/parse-array-declaration tokens)]
      (log/debug "Результат парсинга массива с переменными:" result)
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "unsigned char"))
      (is (:is-array result))
      (is (= (count (get-in result [:init-values :values])) 4))
      (log/debug "Тест завершен успешно"))))

(deftest test-parse-array-zero-size
  (testing "Объявление массива с нулевым размером"
    (log/debug "Начало теста: Массив с нулевым размером")
    (let [tokens (create-tokens
                  [:type-keyword "char"]
                  [:identifier "bar"]
                  [:separator "["]
                  [:int_number "0"]
                  [:separator "]"]
                  [:separator ";"])
          
          result (parser/parse-array-declaration tokens)]
      (log/debug "Результат парсинга массива с нулевым размером:" result)
      (is (= (:type result) :variable-declaration))
      (is (= (:var-type result) "char[]"))
      (is (:is-array result))
      (is (= (get-in (:array-size result) [:value]) "0"))
      (log/debug "Тест завершен успешно"))))

;; (deftest test-parse-multidimensional-array
;;   (testing "Объявление многомерного массива (симуляция)"
;;     (log/debug "Начало теста: Многомерный массив")
;;     (let [tokens (create-tokens
;;                   [:type-keyword "int"]
;;                   [:identifier "matrix"]
;;                   [:separator "["]
;;                   [:int_number "3"]
;;                   [:separator "]"]
;;                   [:separator "["]
;;                   [:int_number "3"]
;;                   [:separator "]"]
;;                   [:separator ";"])
          
;;           result (parser/parse-array-declaration tokens)]
;;       (log/debug "Результат парсинга многомерного массива:" result)
;;       (is (= (:type result) :variable-declaration))
;;       (is (= (:var-type result) "int[][]"))
;;       (is (:is-array result))
;;       (log/debug "Тест завершен успешно"))))

;; (deftest test-parse-invalid-array-declaration
;;   (testing "Ошибка: некорректный синтаксис объявления массива"
;;     (log/debug "Начало теста: Некорректный синтаксис массива")
;;     (let [tokens (create-tokens
;;                   [:type-keyword "int"]
;;                   [:identifier "invalid"]
;;                   [:separator "("]  ; Неправильный разделитель
;;                   [:int_number "10"]
;;                   [:separator "]"]
;;                   [:separator ";"])]
;;       (is (thrown-with-msg?
;;            clojure.lang.ExceptionInfo
;;            #"Некорректный синтаксис объявления массива"
;;            (parser/parse-array-declaration tokens)))
;;       (log/debug "Тест на некорректный синтаксис завершен"))))

;; ;; Тесты для парсинга структур
;; (deftest test-parse-struct-declaration
;;   (testing "Корректное объявление структуры"
;;     (log/debug "Начало теста: Корректное объявление структуры")
;;     (let [tokens (create-tokens
;;                   [:keyword "struct"]
;;                   [:identifier "Point"]
;;                   [:separator "{"]
;;                   [:type-keyword "int"]
;;                   [:identifier "x"]
;;                   [:separator ";"]
;;                   [:type-keyword "int"]
;;                   [:identifier "y"]
;;                   [:separator ";"]
;;                   [:separator "}"])
          
;;           result (parser/parse-struct-declaration tokens)]
;;       (log/debug "Результат парсинга структуры:" result)
;;       (is (= (:type result) :struct-declaration))
;;       (is (= (:name result) "Point"))
;;       (is (vector? (:fields result)))
;;       (is (= (count (:fields result)) 2))
;;       (log/debug "Тест завершен успешно"))))

;; ;; Тесты для парсинга typedef
;; (deftest test-parse-typedef
;;   (testing "Корректное объявление typedef"
;;     (log/debug "Начало теста: Корректное объявление typedef")
;;     (let [tokens (create-tokens
;;                   [:keyword "typedef"]
;;                   [:type-keyword "int"]
;;                   [:identifier "Integer"]
;;                   [:separator ";"])
          
;;           result (parser/parse-typedef tokens)]
;;       (log/debug "Результат парсинга typedef:" result)
;;       (is (= (:type result) :typedef))
;;       (is (= (:original-type result) "int"))
;;       (is (= (:new-type result) "Integer"))
;;       (log/debug "Тест завершен успешно"))))

;; Тесты для парсинга присваивания
(deftest test-parse-assignment
  (testing "Простое присваивание"
    (log/debug "Начало теста: Простое присваивание")
    (let [tokens (create-tokens
                  [:identifier "x"]
                  [:operator "="]
                  [:int_number "42"]
                  [:separator ";"])
          result (parser/parse-assignment tokens)]
      (is (= (:type result) :assignment))
      (is (= (get-in result [:left :value]) "x"))
      (is (= (:operator result) "="))
      (is (= (get-in result [:right :value]) "42"))
      (log/debug "Тест завершен успешно")))

  (testing "Составное присваивание с +="
    (log/debug "Начало теста: Составное присваивание +=")
    (let [tokens (create-tokens
                  [:identifier "counter"]
                  [:operator "+="]
                  [:int_number "1"]
                  [:separator ";"])
          result (parser/parse-assignment tokens)]
      (is (= (:type result) :assignment))
      (is (= (get-in result [:left :value]) "counter"))
      (is (= (:operator result) "+="))
      (is (= (get-in result [:right :value]) "1"))
      (log/debug "Тест завершен успешно")))

  (testing "Присваивание со сложным выражением справа"
    (log/debug "Начало теста: Присваивание со сложным выражением")
    (let [tokens (create-tokens
                  [:identifier "result"]
                  [:operator "="]
                  [:identifier "a"]
                  [:operator "+"]
                  [:identifier "b"]
                  [:operator "*"]
                  [:int_number "2"]
                  [:separator ";"])
          result (parser/parse-assignment tokens)]
      (is (= (:type result) :assignment))
      (is (= (get-in result [:left :value]) "result"))
      (is (= (:operator result) "="))
      (is (map? (:right result)))
      (log/debug "Тест завершен успешно")))

  (testing "Ошибка: отсутствует точка с запятой"
    (log/debug "Начало теста: Отсутствует точка с запятой")
    (let [tokens (create-tokens
                  [:identifier "x"]
                  [:operator "="]
                  [:int_number "42"])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Отсутствует точка с запятой"
           (parser/parse-assignment tokens)))
      (log/debug "Тест на отсутствие точки с запятой завершен")))

  (testing "Ошибка: некорректный оператор присваивания"
    (log/debug "Начало теста: Некорректный оператор")
    (let [tokens (create-tokens
                  [:identifier "x"]
                  [:operator "++"]
                  [:int_number "42"]
                  [:separator ";"])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Некорректное присваивание - ожидается оператор ="
           (parser/parse-assignment tokens)))
      (log/debug "Тест на некорректный оператор завершен"))))