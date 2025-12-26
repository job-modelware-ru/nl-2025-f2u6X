#!/usr/bin/env sh
set -eu

# Docker:
#   cd 05/src
#   docker build -t enpl-scheme-05 .
#   docker run --rm enpl-scheme-05
#
# Локально:
#   guile -L . module-main.scm
#   guile errors-demo.scm
#   guile io-demo.scm

FILE="${1:-module-main.scm}"

if command -v guile >/dev/null 2>&1; then
  case "$FILE" in
    module-main.scm|module-main)
      exec guile -L . module-main.scm
      ;;
    *)
      exec guile "$FILE"
      ;;
  esac
else
  echo "guile не найден. Используйте Docker или установите GNU Guile." >&2
  exit 1
fi


