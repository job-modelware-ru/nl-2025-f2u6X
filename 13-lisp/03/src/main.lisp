
;;; Слайд 4: Переменные: Объявление и связывание
;; Глобальные переменные
(defvar db_connection nil)
(defparameter debug_mode t)
(setq x 15)
(print db_connection)             ; Вывод: NIL
(print debug_mode)                ; Вывод: T
(print x)                         ; Вывод: 15

;; Разница между let и let*
(handler-case
    (let ((a 10)
          (b a))                ; a ещё не связано, b будет неопределённым
      (print b))               ; Вывод: NIL или ошибка (зависит от реализации)
  (error (e) (format t "Error in let: ~a~%" e)))

(let* ((a 10)
       (b a))                 ; a уже связано, b = 10
  (print b))                  ; Вывод: 10

;;; Слайд 5: Области видимости
;; Лексическая область видимости
(let ((x 100))
  (defun test-lexical ()
    x))
(let ((x 200))                    ; Попытка переопределить x
  (print (test-lexical)))         ; Вывод: 100 (лексическое значение)

;; Динамическая область видимости
(defvar *dynamic-var* 50)
(defun test-dynamic ()
  *dynamic-var*)
(let ((*dynamic-var* 75))
  (print (test-dynamic)))         ; Вывод: 75 (динамическое значение)
(print (test-dynamic))            ; Вывод: 50 (исходное значение)

;;; Слайд 6: Функции в Lisp
(defun square (x)
  (* x x))
(print (square 4))                ; Вывод: 16

(print (funcall (lambda (x) (* x x)) 5))  ; Вывод: 25

;;; Слайд 7: Особенности функций
(defun divide (a b)
  (values (truncate a b) (mod a b)))
(multiple-value-bind (quotient remainder) (divide 10 3)
  (print quotient)                ; Вывод: 3
  (print remainder))              ; Вывод: 1

(defun greet (name &optional (greeting "Hello"))
  (format nil "~a, ~a!" greeting name))
(print (greet "Alice"))           ; Вывод: "Hello, Alice!"
(print (greet "Bob" "Hi"))        ; Вывод: "Hi, Bob!"

;;; Слайд 8: Рекурсия
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(print (factorial 5))             ; Вывод: 120

;; Исправленная функция (переименована из-за конфликта с COMMON-LISP:LIST-LENGTH)
(defun my-list-length (lst)
  (if (null lst)
      0
      (+ 1 (my-list-length (cdr lst)))))
(print (my-list-length '(1 2 3 4)))  ; Вывод: 4

(defun factorial-tail (n &optional (acc 1))
  (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* n acc))))
(print (factorial-tail 5))        ; Вывод: 120

;;; Слайд 9: Замыкания
(defun make-counter ()
  (let ((count 0))
    (lambda () (incf count))))
(defvar counter1 (make-counter))
(defvar counter2 (make-counter))
(print (funcall counter1))       ; Вывод: 1
(print (funcall counter1))       ; Вывод: 2
(print (funcall counter2))       ; Вывод: 1

(format t "~%Все примеры завершены.~%")