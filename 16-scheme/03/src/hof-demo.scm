;; hof-demo.scm — функции высшего порядка: compose, partial, filter, fold

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; Композиция функций: (compose f g) => (lambda (x) (f (g x)))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))
(define (sq x) (* x x))
(define inc-then-sq (compose sq inc))
(show "inc-then-sq 4" (inc-then-sq 4)) ; (4+1)^2 = 25

;; Частичное применение (partial): фиксируем первые аргументы
(define (partial2 f a)
  (lambda (b) (f a b)))

(define add10 (partial2 + 10))
(show "add10 7" (add10 7))

;; filter — сделаем сами, чтобы не зависеть от SRFI
(define (filter pred xs)
  (let loop ((ys xs) (acc '()))
    (cond
      ((null? ys) (reverse acc))
      ((pred (car ys)) (loop (cdr ys) (cons (car ys) acc)))
      (else (loop (cdr ys) acc)))))

(show "filter odd? '(1 2 3 4 5)" (filter odd? '(1 2 3 4 5)))

;; foldl — левая свёртка
(define (foldl f init xs)
  (let loop ((acc init) (ys xs))
    (if (null? ys)
        acc
        (loop (f acc (car ys)) (cdr ys)))))

(show "foldl + 0 '(1 2 3 4)" (foldl + 0 '(1 2 3 4)))


