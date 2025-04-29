(ns c51cc.compiler
  "Основной модуль компилятора C51CC"
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [c51cc.logger :as log])
  (:gen-class))

;; Определение спецификации командной строки
(def cli-options
  [["-h" "--help" "Показать справочную информацию"]
   ["-a" "--ast" "Вывести абстрактное синтаксическое дерево (AST)"]
   ["-i" "--IR" "Вывести промежуточное представление (Tree-Address Code)"]
   ["-l" "--lexer" "Вывести результаты лексического анализа"]
   ["-s" "--src" "Вывести исходный код после препроцессора (без комментариев)"]
   ["-p" "--preproc" "Вывести отладочную информацию препроцессора"]
   ["-S" "--ssa" "Вывести значения SSA (Static Single Assignment)"]
   ["-H" "--hir" "Вывести информацию о высокоуровневом промежуточном представлении"]
   ["-L" "--lir" "Вывести информацию о низкоуровневом промежуточном представлении"]
   ["-c" "--colortree" "Вывести значения цветового дерева и регистров"]
   ["-I" "--include PATH" "Путь для включаемых файлов"
    :validate [#(not (str/blank? %)) "Путь включения не может быть пустым"]]
   ["-i" "--ifile FILES" "Входные файлы"
    :parse-fn #(str/split % #",")
    :validate [#(not-empty %) "Должен быть указан хотя бы один входной файл"]]
   ["-o" "--ofile FILE" "Выходной файл"]
   ["-x" "--intelhex" "Вывести результат IntelHex"]
   ["-O" "--optimize LEVEL" "Установить уровень оптимизации"
    :parse-fn #(Integer/parseInt %)
    :validate [#(and (>= % 0) (<= % 3)) "Уровень оптимизации должен быть от 0 до 3"]]])

;; Функция для вывода справочной информации
(defn print-help [summary]
  (println "Использование: c51cc [опции]")
  (println "\nОпции:")
  (println summary))

;; Основная функция обработки аргументов
(defn -main
  "Точка входа компилятора с расширенной обработкой аргументов командной строки"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      ;; Обработка ошибок
      errors 
      (do 
        (log/error "Ошибки при парсинге аргументов:")
        (run! log/error errors)
        (System/exit 1))
      
      ;; Вывод справки
      (:help options)
      (do 
        (print-help summary)
        (System/exit 0))
      
      ;; Основная логика компилятора
      :else
      (do
        (log/info "Начало компиляции")
        (log/debug "Обработанные опции:" options)
        (log/debug "Аргументы:" arguments)
        
        ;; TODO: Добавить основную логику компилятора
        (println "Компиляция...")
        (log/info "Компиляция завершена")))))
