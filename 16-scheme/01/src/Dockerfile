FROM debian:stable-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends guile-3.0 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# По умолчанию запускаем Hello World.
CMD ["guile", "hello-world.scm"]

# Dockerfile для запуска Scheme программ
FROM ubuntu:22.04

# Установка зависимостей
RUN apt-get update && \
    apt-get install -y guile-3.0 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Рабочая директория
WORKDIR /app

# Копирование исходных файлов
COPY hello-world.scm /app/

# Запуск программы
CMD ["guile", "hello-world.scm"]

