# AST Generation and Visualization Scripts

# Функция для выполнения Clojure команд
function Invoke-ClojureCommand {
    param(
        [string]$Command
    )
    
    # Предполагаем, что у вас установлен Clojure и lein
    $output = lein repl :headless :eval "$Command" 2>&1
    return $output
}

# 1. Базовый пример генерации и визуализации AST
function Get-BasicAstExample {
    Write-Host "=== Базовый пример визуализации AST ===" -ForegroundColor Green
    
    $command = @"
(require '[c51cc.ast :as ast])
(let [result (ast/process-c-file 
              :source-path "c51code/simple_example.c"
              :include-path "c51code")]
  (println "Визуализация AST:")
  (println (ast/pretty-print-ast (:ast result)))
  (println "\nАнализ AST:")
  (println (ast/analyze-ast (:ast result))))
"@

    $result = Invoke-ClojureCommand -Command $command
    return $result
}

# 2. Пример создания AST из строки кода
function Get-CodeStringAstExample {
    Write-Host "=== Пример AST из строки кода ===" -ForegroundColor Green
    
    $command = @"
(require '[c51cc.ast :as ast])
(let [code "#include <reg51.h>
            void main(void) { char x = 10; }"
      base-path "."
      ast (ast/generate-ast code base-path)]
  (println "Визуализация AST:")
  (println (ast/pretty-print-ast ast))
  (println "\nАнализ AST:")
  (println (ast/analyze-ast ast)))
"@

    $result = Invoke-ClojureCommand -Command $command
    return $result
}

# 3. Расширенный анализ AST
function Get-AdvancedAstAnalysis {
    Write-Host "=== Расширенный анализ AST ===" -ForegroundColor Green
    
    $command = @"
(require '[c51cc.ast :as ast])
(let [result (ast/process-c-file)
      pretty-print (ast/pretty-print-ast (:ast result))
      ast-analysis (ast/analyze-ast (:ast result))]
  (println "Визуализация AST:")
  (println pretty-print)
  (println "\nАнализ типов узлов:")
  (println (:node-types ast-analysis))
  (println "\nОбщее количество узлов:")
  (println (:total-nodes ast-analysis)))
"@

    $result = Invoke-ClojureCommand -Command $command
    return $result
}

# Главная функция для запуска примеров
function Invoke-AstExamples {
    param(
        [switch]$Basic,
        [switch]$CodeString,
        [switch]$Advanced
    )

    if ($Basic) {
        $basicResult = Get-BasicAstExample
        Write-Host $basicResult
    }

    if ($CodeString) {
        $codeStringResult = Get-CodeStringAstExample
        Write-Host $codeStringResult
    }

    if ($Advanced) {
        $advancedResult = Get-AdvancedAstAnalysis
        Write-Host $advancedResult
    }

    # Если не указаны флаги, выполнить все примеры
    if (-not ($Basic -or $CodeString -or $Advanced)) {
        Get-BasicAstExample
        Get-CodeStringAstExample
        Get-AdvancedAstAnalysis
    }
}

# Справка по использованию скрипта
function Show-AstExamplesHelp {
    Write-Host "Использование скрипта AST Examples:" -ForegroundColor Cyan
    Write-Host "  .\ast-examples.ps1 -Basic       # Базовый пример AST" 
    Write-Host "  .\ast-examples.ps1 -CodeString  # AST из строки кода"
    Write-Host "  .\ast-examples.ps1 -Advanced    # Расширенный анализ AST"
    Write-Host "  .\ast-examples.ps1              # Выполнить все примеры"
}

# Обработка аргументов командной строки
if ($args.Length -eq 0) {
    Invoke-AstExamples
} elseif ($args[0] -eq '-h' -or $args[0] -eq '--help') {
    Show-AstExamplesHelp
} else {
    $params = @{}
    foreach ($arg in $args) {
        switch ($arg) {
            '-Basic' { $params.Basic = $true }
            '-CodeString' { $params.CodeString = $true }
            '-Advanced' { $params.Advanced = $true }
        }
    }
    Invoke-AstExamples @params
} 