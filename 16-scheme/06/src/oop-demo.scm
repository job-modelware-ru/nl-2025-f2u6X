;; oop-demo.scm — ООП в Guile через GOOPS

(use-modules (oop goops))

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; Класс Point с полями x/y
(define-class <point> ()
  (x #:accessor point-x #:init-keyword #:x)
  (y #:accessor point-y #:init-keyword #:y))

;; Метод distance^2 для точки
(define-method (dist2 (p <point>))
  (+ (* (point-x p) (point-x p))
     (* (point-y p) (point-y p))))

(define p (make <point> #:x 3 #:y 4))
(show "point-x" (point-x p))
(show "point-y" (point-y p))
(show "dist2" (dist2 p))


