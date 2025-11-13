FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends smlnj rlwrap ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Копируем только исходники презентации
COPY src/ ./src/

# Запускаем SML/NJ и подаём файл на вход
CMD ["/bin/sh", "-lc", "sml < src/hello.sml"]


