#!/usr/bin/env sh
set -eu

# Docker:
#   cd 06/src
#   docker build -t enpl-scheme-06 .
#   docker run --rm enpl-scheme-06
#
# Локально:
#   guile macros-demo.scm
#   guile oop-demo.scm
#   guile threads-demo.scm

FILE="${1:-macros-demo.scm}"

if command -v guile >/dev/null 2>&1; then
  exec guile "$FILE"
else
  echo "guile не найден. Используйте Docker или установите GNU Guile." >&2
  exit 1
fi


