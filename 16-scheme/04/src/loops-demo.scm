;; loops-demo.scm — итерация: рекурсия, do, именованный let, map/for-each

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; 1) Итерация через хвостовую рекурсию (именованный let)
(define (sum-0..n n)
  (let loop ((i 0) (acc 0))
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc i)))))

(show "sum-0..10" (sum-0..n 10))

;; 2) do — встроенный “цикл”
(define (fact-do n)
  (do ((i n (- i 1))
       (acc 1 (* acc i)))
      ((= i 0) acc)))

(show "fact-do 5" (fact-do 5))

;; 3) map / for-each
(define xs '(1 2 3 4))
(show "map (lambda (x) (* x x)) xs"
      (map (lambda (x) (* x x)) xs))

(display "for-each prints: ")
(for-each (lambda (x)
            (display x)
            (display " "))
          xs)
(newline)

;; 4) foldl (свертка) — сделаем сами, чтобы не зависеть от SRFI
(define (foldl f init xs)
  (let loop ((acc init) (ys xs))
    (if (null? ys)
        acc
        (loop (f acc (car ys)) (cdr ys)))))

(show "foldl + 0 xs" (foldl + 0 xs))


