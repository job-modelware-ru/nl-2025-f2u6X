#!/usr/bin/env sh
set -eu

# Docker:
#   cd 03/src
#   docker build -t enpl-scheme-03 .
#   docker run --rm enpl-scheme-03
#
# Локально:
#   guile functions-demo.scm
#   guile recursion-demo.scm
#   guile closures-demo.scm

FILE="${1:-functions-demo.scm}"

if command -v guile >/dev/null 2>&1; then
  exec guile "$FILE"
else
  echo "guile не найден. Используйте Docker или установите GNU Guile." >&2
  exit 1
fi


