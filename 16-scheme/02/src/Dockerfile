FROM debian:stable-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends guile-3.0 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

CMD ["guile", "types-demo.scm"]


