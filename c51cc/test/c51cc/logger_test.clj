(ns c51cc.logger_test
  "Тестовый модуль для проверки функциональности системы логирования.
   Охватывает различные сценарии использования и проверки уровней логирования."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [c51cc.logger :as log])
  (:import (java.io ByteArrayOutputStream PrintStream)))

(defn- capture-stdout 
  "Захватывает стандартный вывод для тестирования логирования.
   
   Параметры:
   - func: Функция, которую необходимо выполнить
   
   Возвращает вектор с результатом выполнения и захваченным выводом."
  [func]
  (let [output (ByteArrayOutputStream.)
        previous System/out
        ps (PrintStream. output true)]
    (System/setOut ps)
    (try
      (let [result (func)]
        [result (str/trim (str output))])
      (finally 
        (System/setOut previous)))))

(deftest test-log-levels
  "Тестирование различных уровней логирования с проверкой вывода."
  (testing "Уровень SILENT - никакие сообщения не должны выводиться"
    (log/set-debug-level! :SILENT)
    (let [[_ output] (capture-stdout 
                      #(do 
                         (log/trace "Трассировочное сообщение")
                         (log/debug "Отладочное сообщение")
                         (log/info "Информационное сообщение")))]
      (is (= "" output))))
  
  (testing "Уровень INFO - только информационные сообщения"
    (log/set-debug-level! :INFO)
    (let [[_ output] (capture-stdout 
                      #(do 
                         (log/trace "Трассировочное сообщение")
                         (log/debug "Отладочное сообщение")
                         (log/info "Информационное сообщение")))]
      (is (= "INFO: Информационное сообщение" output))))
  
  (testing "Уровень DEBUG - информационные и отладочные сообщения"
    (log/set-debug-level! :DEBUG)
    (let [[_ output] (capture-stdout 
                      #(do 
                         (log/trace "Трассировочное сообщение")
                         (log/debug "Отладочное сообщение")
                         (log/info "Информационное сообщение")))]
      (is (= (str "INFO: Информационное сообщение\n"
                  "DEBUG: Отладочное сообщение") 
             output))))
  
  (testing "Уровень TRACE - все сообщения"
    (log/set-debug-level! :TRACE)
    (let [[_ output] (capture-stdout 
                      #(do 
                         (log/trace "Трассировочное сообщение")
                         (log/debug "Отладочное сообщение")
                         (log/info "Информационное сообщение")))]
      (is (= (str "INFO: Информационное сообщение\n"
                  "DEBUG: Отладочное сообщение\n"
                  "TRACE: Трассировочное сообщение") 
             output))))
  
  (testing "Проверка установки уровня логирования"
    (log/set-debug-level! :DEBUG)
    (is (= :DEBUG @#'c51cc.logger/*current-debug-level*))))

(deftest test-log-macro
  "Тестирование макроса логирования с различными входными данными"
  (testing "Логирование с несколькими аргументами"
    (log/set-debug-level! :DEBUG)
    (let [[_ output] (capture-stdout 
                      #(log/debug "Первый аргумент " "Второй аргумент " 42))]
      (is (= "DEBUG: Первый аргумент Второй аргумент 42" 
             output)))))