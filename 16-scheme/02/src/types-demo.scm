;; types-demo.scm — типы данных, операции и сравнения (GNU Guile)

(define (println x)
  (display x)
  (newline))

(define (show label value)
  (display label)
  (display ": ")
  (write value)   ; write показывает кавычки строк и т.п.
  (newline))

;; Примитивные типы
(show "number" 42)
(show "boolean" #t)
(show "char" #\A)
(show "string" "scheme")
(show "symbol" 'alpha)

;; Составные структуры
(show "pair" (cons 1 2))
(show "list" (list 1 2 3))
(show "vector" (vector 1 2 3))

;; Процедуры — тоже значения (first-class)
(define inc (lambda (x) (+ x 1)))
(show "procedure?" (procedure? inc))
(show "(inc 41)" (inc 41))

(newline)
(println "== Сравнения ==")

;; eq? — “одинаковый объект?” (часто для символов, и идентичности)
;; equal? — структурное равенство (списки/строки и т.д.)
(show "eq? symbols" (eq? 'alpha 'alpha))
(show "eq? strings (implementation-defined)" (eq? "a" "a"))
(show "equal? strings" (equal? "a" "a"))
(show "equal? lists" (equal? '(1 2) (list 1 2)))

(newline)
(println "== Преобразования ==")
(show "string->number" (string->number "123"))
(show "number->string" (number->string 123))
(show "symbol->string" (symbol->string 'hello))
(show "string->symbol" (string->symbol "hello"))

(newline)
(println "== Мутабельность (важно для семантики) ==")
(define xs (list 1 2 3))
(show "xs before" xs)
;; set-car! мутирует пару (голову списка)
(set-car! xs 999)
(show "xs after set-car!" xs)


