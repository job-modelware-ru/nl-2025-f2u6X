# Базовый образ с SBCL и Quicklisp для разработки
FROM daewok/lisp-devel:base

# Устанавливаем рабочую директорию в контейнере
WORKDIR /project

# Копируем ваш Lisp-код в контейнер
COPY . /project

# Команда по умолчанию: запуск SBCL с вашим скриптом
CMD ["sbcl", "--script", "main.lisp"] 
