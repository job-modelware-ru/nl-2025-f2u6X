#!/usr/bin/env sh
set -eu

# Docker:
#   cd 04/src
#   docker build -t enpl-scheme-04 .
#   docker run --rm enpl-scheme-04
#
# Локально:
#   guile control-demo.scm
#   guile loops-demo.scm

FILE="${1:-control-demo.scm}"

if command -v guile >/dev/null 2>&1; then
  exec guile "$FILE"
else
  echo "guile не найден. Используйте Docker или установите GNU Guile." >&2
  exit 1
fi


