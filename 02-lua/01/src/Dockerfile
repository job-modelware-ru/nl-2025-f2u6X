FROM alpine:3.19
RUN apk add --no-cache lua5.4
WORKDIR /app
COPY hello_world.lua .
CMD ["lua5.4", "hello_world.lua"]
