;; errors-demo.scm — ошибки/исключения, assert-подобные проверки

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; Простейший assert: падаем с error, если условие ложно
(define (assert pred msg)
  (if (not pred)
      (error msg)
      #t))

(show "assert #t" (assert #t "should not fail"))

;; Пример функции, которая может “упасть”
(define (safe-div a b)
  (assert (not (= b 0)) "division by zero")
  (/ a b))

(show "safe-div 10 2" (safe-div 10 2))

;; Перехват исключений (Guile): catch
;; catch принимает:
;;  - key (обычно #t чтобы ловить всё),
;;  - thunk (что выполнить),
;;  - handler (что делать при исключении)
(define (try thunk)
  (catch #t
    thunk
    (lambda (key . args)
      (display "caught exception: ")
      (write key)
      (display " args=")
      (write args)
      (newline)
      'error)))

(show "try safe-div 1 0" (try (lambda () (safe-div 1 0))))


