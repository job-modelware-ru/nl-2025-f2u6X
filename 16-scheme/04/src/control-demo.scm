;; control-demo.scm — условные конструкции и блоки

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; 1) if — базовая развилка
(define (sign x)
  (if (< x 0) 'neg 'non-neg))

(show "sign -5" (sign -5))
(show "sign  0" (sign 0))

;; 2) cond — цепочка условий
(define (classify x)
  (cond
    ((< x 0) 'negative)
    ((= x 0) 'zero)
    (else 'positive)))

(show "classify -1" (classify -1))
(show "classify  0" (classify 0))
(show "classify  7" (classify 7))

;; 3) case — ветвление по значению (обычно по символам/малому набору)
(define (weekday-type day)
  (case day
    ((sat sun) 'weekend)
    ((mon tue wed thu fri) 'workday)
    (else 'unknown)))

(show "weekday-type 'sat" (weekday-type 'sat))
(show "weekday-type 'mon" (weekday-type 'mon))

;; 4) and/or — короткое замыкание
(define (between? x lo hi)
  (and (>= x lo) (<= x hi)))

(show "between? 5 1 10" (between? 5 1 10))
(show "between? 0 1 10" (between? 0 1 10))

;; 5) begin — блок: выполнить несколько выражений и вернуть последнее
(define (demo-begin)
  (begin
    (display "line1")
    (newline)
    (display "line2")
    (newline)
    'done))

(show "demo-begin returns" (demo-begin))


