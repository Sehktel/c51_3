# AST Tools PowerShell Script

## Описание

Расширенный PowerShell-скрипт для анализа и манипуляции Абстрактного Синтаксического Дерева (AST) в проектах на языке C51.

## Требования

- PowerShell 7+
- Clojure
- Leiningen (lein)
- Проект c51cc

## Возможности

1. Анализ файлов
2. Поиск узлов AST
3. Генерация подробных отчетов

## Использование

### Анализ файла

```powershell
# Базовый анализ файла
.\ast-tools.ps1 analyze -FilePath c51code/example.c

# Анализ с указанием пути включения
.\ast-tools.ps1 analyze -FilePath c51code/example.c -IncludePath include
```

### Поиск узлов AST

```powershell
# Поиск функций по умолчанию
.\ast-tools.ps1 find-nodes -FilePath c51code/example.c

# Поиск узлов определенного типа
.\ast-tools.ps1 find-nodes -FilePath c51code/example.c -NodeType variable-declaration
```

### Генерация отчета

```powershell
# Генерация отчета с именем по умолчанию
.\ast-tools.ps1 generate-report -FilePath c51code/example.c

# Генерация отчета с указанием пути вывода
.\ast-tools.ps1 generate-report -FilePath c51code/example.c -OutputPath my_report.txt
```

### Справка

```powershell
.\ast-tools.ps1 -h
# или
.\ast-tools.ps1 --help
```

## Типы узлов AST

- `function-declaration`
- `variable-declaration`
- `function-call`
- `control-flow`
- `expression`
- `preprocessor-directive`
- `include-directive`

## Примечания

- Скрипт использует `lein repl` для выполнения Clojure-команд
- Путь к файлам может потребовать корректировки в зависимости от структуры вашего проекта

## Лицензия

[Укажите вашу лицензию]

## Автор

[Ваше имя] 