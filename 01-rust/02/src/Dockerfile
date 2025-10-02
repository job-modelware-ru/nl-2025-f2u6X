FROM rust:1.81

WORKDIR /usr/src/app
COPY . .

RUN cargo build --release

CMD ["./target/release/rust_basics"]
