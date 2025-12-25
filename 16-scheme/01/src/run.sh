#!/usr/bin/env sh
set -eu

# Локальный запуск (требуется установленный guile):
#   guile hello-world.scm
#
# Docker-запуск:
#   docker build -t enpl-scheme-01 .
#   docker run --rm enpl-scheme-01

if command -v guile >/dev/null 2>&1; then
  exec guile hello-world.scm
else
  echo "guile не найден. Установите GNU Guile или используйте Docker (см. комментарии в run.sh)." >&2
  exit 1
fi

#!/bin/bash
# Скрипт для запуска программы hello-world.scm

# Проверка наличия Guile
if command -v guile &> /dev/null; then
    echo "Запуск через Guile..."
    guile hello-world.scm
elif command -v racket &> /dev/null; then
    echo "Запуск через Racket..."
    racket hello-world.scm
elif command -v csi &> /dev/null; then
    echo "Запуск через Chicken Scheme..."
    csi hello-world.scm
else
    echo "Ошибка: не найдена установка Scheme"
    echo "Установите один из интерпретаторов: Guile, Racket или Chicken Scheme"
    exit 1
fi

