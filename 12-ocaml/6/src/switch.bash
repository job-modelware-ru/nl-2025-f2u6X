# инициализировать opam (если ещё не инициализировали)
opam init
eval $(opam env)

# создать switch с конкретной версией OCaml
opam switch create 4.14.0 ocaml-base-compiler.4.14.0
eval $(opam env)

# установить зависимости из файла opam
opam install . --deps-only