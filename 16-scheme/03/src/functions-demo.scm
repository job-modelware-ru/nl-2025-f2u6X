;; functions-demo.scm — функции: аргументы, функции как значения, высший порядок

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; 1) Обычная функция (синтаксический сахар)
(define (add a b)
  (+ a b))

(show "add 2 3" (add 2 3))

;; 2) lambda — “анонимная” функция
(define mul (lambda (a b) (* a b)))
(show "mul 6 7" (mul 6 7))

;; 3) Вариативные аргументы (rest args)
;; (lambda (a b . rest) ...) — rest это список “оставшихся” аргументов
(define (sum2+ a b . rest)
  (let loop ((acc (+ a b)) (xs rest))
    (if (null? xs)
        acc
        (loop (+ acc (car xs)) (cdr xs)))))

(show "sum2+ 1 2" (sum2+ 1 2))
(show "sum2+ 1 2 3 4 5" (sum2+ 1 2 3 4 5))

;; 4) Функции высшего порядка: передаём функцию в функцию
(define (apply-twice f x)
  (f (f x)))

(define inc (lambda (x) (+ x 1)))
(show "apply-twice inc 10" (apply-twice inc 10))

;; map: применяет f к каждому элементу списка
(show "map inc '(1 2 3)" (map inc '(1 2 3)))

;; 5) Функция может возвращать функцию (подготовка к замыканиям)
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add10 (make-adder 10))
(show "add10 7" (add10 7))


