FROM dlang2/dmd-ubuntu AS builder

WORKDIR /app
COPY . .
RUN dmd hello_world.d -ofhello_world -O -release -inline -boundscheck=off

FROM ubuntu:22.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libc6 \
    libstdc++6 \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -m -u 1000 appuser

WORKDIR /app
COPY --from=builder /app/hello_world .

USER appuser

CMD ["./hello_world"]