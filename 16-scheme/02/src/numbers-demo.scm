;; numbers-demo.scm — числа, точность и “exact/inexact” (Guile)

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; В Scheme числа могут быть точными (exact) и приближенными (inexact).
;; Конкретное поведение зависит от реализации, но концепция распространена.

(show "exact? 1" (exact? 1))
(show "exact? 1.0" (exact? 1.0))
(show "inexact? 1.0" (inexact? 1.0))

;; Деление может давать рациональные числа (exact), а может — inexact,
;; зависит от литералов/операций.
(show "(/ 7 2)" (/ 7 2))
(show "(/ 7 2.0)" (/ 7 2.0))

;; Приведение к inexact (часто используется для работы с float)
(show "(exact->inexact 1/3)" (exact->inexact 1/3))

;; Осторожно с сравнениями float
(define a 0.1)
(define b 0.2)
(define c (+ a b))
(show "0.1 + 0.2" c)
(show "(= (+ 0.1 0.2) 0.3)" (= c 0.3))


