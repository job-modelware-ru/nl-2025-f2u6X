
FROM rust:1.75

WORKDIR /app

COPY main.rs .

RUN rustc main.rs -O -o app

CMD ["./app"]
