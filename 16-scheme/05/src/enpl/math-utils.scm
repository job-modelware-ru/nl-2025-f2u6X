;; enpl/math-utils.scm — пример модуля Guile
;;
;; Важно: путь файла соответствует имени модуля (enpl math-utils)
;; Чтобы импортировать локальные модули, запускайте Guile с: guile -L .

(define-module (enpl math-utils)
  #:export (square cube))

(define (square x) (* x x))
(define (cube x) (* x x x))


