;; recursion-demo.scm — рекурсия и хвостовая рекурсия (TCO)

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; Обычная рекурсия (не хвостовая)
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

;; Хвостовая рекурсия: рекурсивный вызов — последнее действие
(define (fact-tail n)
  (let loop ((k n) (acc 1))
    (if (= k 0)
        acc
        (loop (- k 1) (* acc k)))))

(show "fact 5" (fact 5))
(show "fact-tail 5" (fact-tail 5))

;; Пример “цикла” через именованный let
(define (sum-to n)
  (let loop ((i 0) (acc 0))
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc i)))))

(show "sum-to 10" (sum-to 10))


