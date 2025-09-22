# Этап 1: сборка
FROM rust:latest AS builder
WORKDIR /usr/src/app
COPY Cargo.toml .
COPY src ./src
# Сборка релизной версии
RUN cargo build --release
# Этап 2: запуск
FROM debian:bookworm-slim
WORKDIR /usr/local/bin
COPY --from=builder /usr/src/app/target/release/rust-hello .
# Запускаем приложение
CMD ["./rust-hello"]
