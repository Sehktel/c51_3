# AST Examples PowerShell Script

## Описание

Этот PowerShell-скрипт предоставляет утилиты для генерации и визуализации Абстрактного Синтаксического Дерева (AST) для C51 проектов.

## Требования

- PowerShell 7+
- Clojure
- Leiningen (lein)
- Проект c51cc

## Использование

### Базовые команды

```powershell
# Выполнить все примеры
.\ast-examples.ps1

# Базовый пример AST
.\ast-examples.ps1 -Basic

# AST из строки кода
.\ast-examples.ps1 -CodeString

# Расширенный анализ AST
.\ast-examples.ps1 -Advanced
```

### Справка

```powershell
.\ast-examples.ps1 -h
# или
.\ast-examples.ps1 --help
```

## Функции

1. `Get-BasicAstExample`: Генерация AST из файла
2. `Get-CodeStringAstExample`: Создание AST из строки кода
3. `Get-AdvancedAstAnalysis`: Подробный анализ AST

## Примечания

- Скрипт использует `lein repl` для выполнения Clojure-команд
- Путь к файлам может потребовать корректировки в зависимости от структуры вашего проекта

## Лицензия

[Укажите вашу лицензию]

## Автор

[Ваше имя] 