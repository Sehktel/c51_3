#!/bin/bash

# Скрипт для запуска тестов с настройкой переменных окружения

# Базовый путь проекта
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Путь к тестовому файлу по умолчанию
DEFAULT_TEST_FILE="c51code/simple_example.c"

# Путь к директории включений по умолчанию
DEFAULT_INCLUDE_PATH="c51code"

# Установка переменных окружения
export TEST_FILE="${TEST_FILE:-$PROJECT_ROOT/$DEFAULT_TEST_FILE}"
export INCLUDE_PATH="${INCLUDE_PATH:-$PROJECT_ROOT/$DEFAULT_INCLUDE_PATH}"

# Вывод информации о тестировании
echo "=== Параметры тестирования ==="
echo "Тестовый файл: $TEST_FILE"
echo "Путь включений: $INCLUDE_PATH"
echo "========================="

# Запуск тестов
lein test :only c51cc.ast_test 