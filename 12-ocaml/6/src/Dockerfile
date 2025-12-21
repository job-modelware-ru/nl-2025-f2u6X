FROM ocaml/opam:alpine-ocaml-5.1 AS build

WORKDIR /app
COPY my_project.opam dune-project ./
RUN opam install . --deps-only -y

COPY . .
RUN opam exec -- dune build --profile=release

FROM alpine:latest

RUN apk add --no-cache libev gmp

COPY --from=build /app/_build/default/bin/main.exe /usr/local/bin/myapp

CMD ["/usr/local/bin/myapp"]