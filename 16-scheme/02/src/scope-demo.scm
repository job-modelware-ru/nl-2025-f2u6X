;; scope-demo.scm — переменные и области видимости

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

(define x 10)
(show "global x" x)

;; let создаёт новую лексическую область
(let ((x 20) (y 30))
  (show "inside let: x" x)
  (show "inside let: y" y)
  ;; set! меняет “текущую” привязку (здесь — x внутри let)
  (set! x 21)
  (show "inside let after (set! x 21)" x))

(show "global x after let" x)

;; let* — последовательные привязки (каждая видит предыдущие)
(let* ((a 1)
       (b (+ a 10)))
  (show "let*: a" a)
  (show "let*: b" b))

;; letrec — для взаимной рекурсии (функции видят друг друга)
(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (show "even? 10" (even? 10))
  (show "odd?  10" (odd? 10)))


