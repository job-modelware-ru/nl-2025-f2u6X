;; fp-demo.scm — FP-пайплайны на списках: map/filter/fold, без SRFI

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

(define (filter pred xs)
  (let loop ((ys xs) (acc '()))
    (cond
      ((null? ys) (reverse acc))
      ((pred (car ys)) (loop (cdr ys) (cons (car ys) acc)))
      (else (loop (cdr ys) acc)))))

(define (foldl f init xs)
  (let loop ((acc init) (ys xs))
    (if (null? ys)
        acc
        (loop (f acc (car ys)) (cdr ys)))))

(define xs '(1 2 3 4 5 6))

;; “Пайплайн”: взять нечётные, возвести в квадрат, сложить
(define odds (filter odd? xs))
(define squares (map (lambda (x) (* x x)) odds))
(define sum (foldl + 0 squares))

(show "xs" xs)
(show "odds" odds)
(show "squares" squares)
(show "sum" sum)


