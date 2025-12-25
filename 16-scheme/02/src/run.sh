#!/usr/bin/env sh
set -eu

# Локально (если установлен guile):
#   guile types-demo.scm
#
# Docker:
#   docker build -t enpl-scheme-02 .
#   docker run --rm enpl-scheme-02
#
# Можно запускать и другие демо:
#   guile scope-demo.scm
#   guile records-demo.scm

FILE="${1:-types-demo.scm}"

if command -v guile >/dev/null 2>&1; then
  exec guile "$FILE"
else
  echo "guile не найден. Используйте Docker (см. run.sh) или установите GNU Guile." >&2
  exit 1
fi


