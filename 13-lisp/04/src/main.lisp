;; Примеры if, when, unless

;; if: Проверка условия с then и else ветками
(let ((x 5))
  (print (if (> x 0) "positive" "negative")))  ; Вывод: "positive"

;; when: Выполнение тела, если условие истинно
(defvar *debug* t)  ; Глобальная переменная для демонстрации
(when *debug*
  (print "Debug mode is active"))  ; Вывод: "Debug mode is active"

;; unless: Выполнение тела, если условие ложно
(defun my-file-exists-p (file)
  (probe-file file))  ; Функция-заглушка для проверки существования файла
(let ((file "nonexistent.txt"))
  (unless (my-file-exists-p file)
    (print (format nil "File ~a does not exist, creating..." file))))
    
; Вывод: "File nonexistent.txt does not exist, creating..."

;; Примеры cond и case

;; cond: Последовательная проверка условий
(defun number-type (n)
  (cond ((> n 0) "positive")
        ((< n 0) "negative")
        (t "zero")))
(print (number-type 10))  ; Вывод: "positive"
(print (number-type -5))  ; Вывод: "negative"
(print (number-type 0))   ; Вывод: "zero"

;; case: Выбор по конкретным значениям
(defun day-of-week (day)
  (case day
    (1 "Monday")
    (2 "Tuesday")
    (3 "Wednesday")
    (otherwise "Unknown day")))
(print (day-of-week 1))   ; Вывод: "Monday"
(print (day-of-week 4))   ; Вывод: "Unknown day"

;; Примеры loop, dotimes, dolist, do

;; loop: Мощный макрос для сложных циклов
(defun sum-squares (n)
  (loop for i from 1 to n
        sum (* i i)))
(print (sum-squares 3))   ; Вывод: 14 (1^2 + 2^2 + 3^2 = 1 + 4 + 9)

;; dotimes: Цикл по числам
(dotimes (i 5)
  (print i))          ; Вывод: 0 1 2 3 4

;; dolist: Цикл по элементам списка
(dolist (item '(a b c))
  (print item))           ; Вывод: a b c

;; do: Универсальный цикл
(defun factorial-do (n)
  (do ((i 1 (1+ i))
       (result 1 (* result i)))
      ((> i n) result)))
(print (factorial-do 5))  ; Вывод: 120 (5!)

;; Примеры error, ignore-errors, handler-case, unwind-protect

;; error: Выбрасывание ошибки
(defun divide-safe (a b)
  (if (zerop b)
      (error "Division by zero")
      (/ a b)))
(handler-case
    (print (divide-safe 10 0))  ; Вызовет ошибку
  (error (e) (format t "Caught error: ~a~%" e))) 
 ; Вывод: "Caught error: Division by zero"

;; ignore-errors: Игнорирование ошибок
(print (ignore-errors (divide-safe 10 0)))  ; Вывод: NIL

;; handler-case: Обработка конкретных ошибок
(handler-case
    (divide-safe 10 0)
  (division-by-zero (e)
    (print "Caught division by zero"))
  (error (e)
    (format t "Other error: ~a~%" e)))  ; Вывод: "Caught division by zero"

;; unwind-protect: Обеспечение выполнения cleanup-кода
(defun cleanup-example ()
  (unwind-protect
       (progn
         (print "Performing operation...")
         (error "An error occurred"))
    (print "Cleaning up...")))
(handler-case
    (cleanup-example)
  (error (e) (format t "Caught error: ~a~%" e)))  ;
   Вывод: "Performing operation..." "Cleaning up..." "Caught error: An error occurred"

;; Примеры trace, break, step

;; trace: Трассировка функции
(defun my-square (x)
  (* x x))
(trace my-square)
(print (my-square 4))  ; Вывод в REPL: 
                       ; 0: (MY-SQUARE 4)
                       ; 0: MY-SQUARE returned 16
                       ; 16
(untrace my-square)    ; Отключение трассировки

;; break: Точка останова
(defun break-example (x)
  (break "Breakpoint at x=~a" x)  ; В REPL: вызывает отладчик с сообщением "Breakpoint at x=~a"
  (* x 2))
(break-example 5)

;; step: Пошаговая отладка
;; step работает только в REPL. Пример: (step (my-square 4))
(defun my-square (x)
  (* x x))
(step (my-square 4))


;; Примеры assert и check-type

;; assert: Проверка условий
(defun safe-divide (a b)
  (assert (not (zerop b)) (b) "Cannot divide by zero")
  (/ a b))
(print (safe-divide 10 2))  ; Вывод: 5
(handler-case
    (print (safe-divide 10 0))  ; Вызовет ошибку
  (error (e) (format t "Caught error: ~a~%" e)))  
; Вывод: "Caught error: Cannot divide by zero"

;; check-type: Проверка типа
(defun process-number (x)
  (check-type x number "a number")
  (* x 2))
(print (process-number 5))  ; Вывод: 10
(handler-case
    (print (process-number "string"))  ; Вызовет ошибку
  (error (e) (format t "Caught error: ~a~%" e)))  
; Вывод: "Caught error: The value string is not of type number"
