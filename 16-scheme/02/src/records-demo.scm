;; records-demo.scm — “пользовательские типы” (records) через SRFI-9 (Guile)

(use-modules (srfi srfi-9))

(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))

(display "point? => ")
(write (point? p))
(newline)

(display "x => ")
(write (point-x p))
(newline)

(display "y => ")
(write (point-y p))
(newline)

(display "distance^2 => ")
(write (+ (* (point-x p) (point-x p))
          (* (point-y p) (point-y p))))
(newline)


