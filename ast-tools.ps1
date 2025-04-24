# Advanced AST Manipulation and Analysis Tools

# Функция для выполнения Clojure команд с расширенной обработкой
function Invoke-ClojureCommand {
    param(
        [string]$Command,
        [switch]$Verbose
    )
    
    try {
        $output = lein repl :headless :eval "$Command" 2>&1
        
        if ($Verbose) {
            Write-Host "=== Clojure Command Output ===" -ForegroundColor Green
            Write-Host $output
        }
        
        return $output
    }
    catch {
        Write-Host "Ошибка при выполнении Clojure команды:" -ForegroundColor Red
        Write-Host $_.Exception.Message
        return $null
    }
}

# Функция для парсинга и анализа конкретного файла
function Analyze-CFile {
    param(
        [string]$FilePath,
        [string]$IncludePath = "."
    )

    Write-Host "Анализ файла: $FilePath" -ForegroundColor Cyan

    $command = @"
(require '[c51cc.ast :as ast]
         '[c51cc.lexer :as lexer]
         '[c51cc.preprocessor :as preprocessor])

(let [content (slurp "$FilePath")
      cleaned-content (preprocessor/remove-comments content)
      tokens (lexer/tokenize cleaned-content)
      ast (ast/generate-ast content "$IncludePath")
      analysis (ast/analyze-ast ast)]
  (println "=== Статистика токенов ===")
  (println "Количество токенов:" (count tokens))
  (println "\n=== Анализ AST ===")
  (println "Типы узлов:" (:node-types analysis))
  (println "Общее количество узлов:" (:total-nodes analysis))
  (println "\n=== Первые 5 токенов ===")
  (println (take 5 tokens)))
"@

    $result = Invoke-ClojureCommand -Command $command -Verbose
    return $result
}

# Функция для поиска специфических узлов в AST
function Find-AstNodes {
    param(
        [string]$FilePath,
        [string]$NodeType = "function-declaration"
    )

    Write-Host "Поиск узлов типа: $NodeType в файле $FilePath" -ForegroundColor Cyan

    $command = @"
(require '[c51cc.ast :as ast])

(let [result (ast/process-c-file 
              :source-path "$FilePath")
      ast (:ast result)
      matching-nodes (filter #(= (:type %) :$NodeType) (:nodes ast))]
  (println "=== Найденные узлы типа $NodeType ===")
  (doseq [node matching-nodes]
    (println "Имя:" (:name node))
    (println "Тип возвращаемого значения:" (:return-type node))
    (println "---")))
"@

    $result = Invoke-ClojureCommand -Command $command -Verbose
    return $result
}

# Функция для генерации отчета по AST
function Generate-AstReport {
    param(
        [string]$FilePath,
        [string]$OutputPath = "ast_report.txt"
    )

    Write-Host "Генерация отчета по AST для файла: $FilePath" -ForegroundColor Cyan

    $command = @"
(require '[c51cc.ast :as ast]
         '[clojure.pprint :as pprint])

(let [result (ast/process-c-file 
              :source-path "$FilePath")
      ast (:ast result)
      pretty-print (ast/pretty-print-ast ast)
      analysis (ast/analyze-ast ast)]
  (with-open [w (clojure.java.io/writer "$OutputPath")]
    (binding [*out* w]
      (println "=== Отчет по AST ===")
      (println "Исходный файл:" "$FilePath")
      (println "\n=== Визуализация AST ===")
      (pprint/pprint pretty-print)
      (println "\n=== Анализ AST ===")
      (pprint/pprint analysis)))
  (println "Отчет сохранен в $OutputPath"))
"@

    $result = Invoke-ClojureCommand -Command $command -Verbose
    return $result
}

# Главная функция для выбора действия
function Invoke-AstTool {
    param(
        [string]$Action,
        [string]$FilePath,
        [string]$IncludePath = ".",
        [string]$NodeType,
        [string]$OutputPath
    )

    switch ($Action) {
        "analyze" { 
            Analyze-CFile -FilePath $FilePath -IncludePath $IncludePath 
        }
        "find-nodes" { 
            Find-AstNodes -FilePath $FilePath -NodeType $NodeType 
        }
        "generate-report" { 
            Generate-AstReport -FilePath $FilePath -OutputPath $OutputPath 
        }
        default { 
            Write-Host "Неверное действие. Используйте: analyze, find-nodes, generate-report" -ForegroundColor Red 
        }
    }
}

# Справка по использованию скрипта
function Show-AstToolsHelp {
    Write-Host "Использование скрипта AST Tools:" -ForegroundColor Cyan
    Write-Host "  .\ast-tools.ps1 analyze -FilePath <path> [-IncludePath <path>]"
    Write-Host "  .\ast-tools.ps1 find-nodes -FilePath <path> [-NodeType <type>]"
    Write-Host "  .\ast-tools.ps1 generate-report -FilePath <path> [-OutputPath <path>]"
    Write-Host ""
    Write-Host "Примеры:" -ForegroundColor Green
    Write-Host "  .\ast-tools.ps1 analyze -FilePath c51code/example.c"
    Write-Host "  .\ast-tools.ps1 find-nodes -FilePath c51code/example.c -NodeType function-declaration"
    Write-Host "  .\ast-tools.ps1 generate-report -FilePath c51code/example.c -OutputPath report.txt"
}

# Обработка аргументов командной строки
if ($args.Length -eq 0 -or $args[0] -eq '-h' -or $args[0] -eq '--help') {
    Show-AstToolsHelp
} else {
    $action = $args[0]
    $params = @{}
    
    for ($i = 1; $i -lt $args.Length; $i += 2) {
        switch ($args[$i]) {
            "-FilePath" { $params.FilePath = $args[$i + 1] }
            "-IncludePath" { $params.IncludePath = $args[$i + 1] }
            "-NodeType" { $params.NodeType = $args[$i + 1] }
            "-OutputPath" { $params.OutputPath = $args[$i + 1] }
        }
    }
    
    Invoke-AstTool -Action $action @params
} 