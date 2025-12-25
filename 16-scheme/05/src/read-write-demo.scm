;; read-write-demo.scm — read/write как “формат данных” на S-выражениях

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; Сериализация: write печатает так, чтобы можно было прочитать обратно
(define data '(user (name . "Alice") (age . 20) (tags "fp" "lisp")))

(define s
  (call-with-output-string
    (lambda (port)
      (write data port))))

(show "serialized" s)

;; Десериализация: read читает S-выражение из строки/порта
(define parsed
  (call-with-input-string s
    (lambda (port)
      (read port))))

(show "parsed" parsed)
(show "equal? parsed data" (equal? parsed data))


