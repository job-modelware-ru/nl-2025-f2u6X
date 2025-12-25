;; module-main.scm — импорт/экспорт, пространства имён (modules)

(use-modules (enpl math-utils))

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

(show "square 5" (square 5))
(show "cube 3" (cube 3))


