FROM rust:latest
WORKDIR /usr/src/concurrency_rust
COPY . .
RUN cargo build --release
CMD ["./target/release/concurrency_rust"]
