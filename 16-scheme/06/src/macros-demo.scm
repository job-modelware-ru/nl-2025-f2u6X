;; macros-demo.scm — макросы (syntax-rules) и “мини-DSL”

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; 1) Пример макроса unless (синтаксический сахар)
(define-syntax unless
  (syntax-rules ()
    ((unless test expr ...)
     (if (not test)
         (begin expr ...)
         #t))))

(define x 0)
(unless (> x 0)
  (set! x 10)
  (display "unless ran")
  (newline))

(show "x" x)

;; 2) “ZZZZZZ-ориентированное программирование” как DSL:
;; Мини-DSL для описания правил переписывания/преобразований.
;; Это учебный пример: “язык внутри языка” благодаря макросам.

(define-syntax rule
  (syntax-rules (=>)
    ((rule name (pred x) => (action x))
     (define (name x)
       (if (pred x) (action x) x)))))

(rule normalize-hello
  ((lambda (s) (equal? s "HELLO")) s)
  =>
  ((lambda (s) "hello") s))

(show "normalize-hello \"HELLO\"" (normalize-hello "HELLO"))
(show "normalize-hello \"Hi\"" (normalize-hello "Hi"))


