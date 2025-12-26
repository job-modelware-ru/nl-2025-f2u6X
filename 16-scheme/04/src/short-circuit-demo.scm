;; short-circuit-demo.scm — and/or как управляющие формы, cond с =>

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; and/or возвращают не обязательно #t/#f, а последнее вычисленное значение
(show "(and #t 10)" (and #t 10))
(show "(and #f 10)" (and #f 10))
(show "(or #f 42)" (or #f 42))
(show "(or  0  42)" (or 0 42)) ; 0 — истинное значение в Scheme

;; Частая идиома: (and x (f x)) — вызвать f только если x “не ложь”
(define maybe-name "Alice")
(show "maybe-name length"
      (and maybe-name (string-length maybe-name)))

(define maybe-none #f)
(show "maybe-none length"
      (and maybe-none (string-length maybe-none)))

;; cond с => : если тест дал значение v, то (=> proc) вызовет proc на v
(define (string->maybe-number s)
  (cond
    ((string->number s) => (lambda (n) (+ n 1))) ; если распарсилось, прибавим 1
    (else #f)))

(show "string->maybe-number \"10\"" (string->maybe-number "10"))
(show "string->maybe-number \"nope\"" (string->maybe-number "nope"))


