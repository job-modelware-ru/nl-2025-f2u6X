;; examples.lisp
;; Примеры программ на Common Lisp для каждого слайда презентации "2. Lisp - данные и типы.pptx"
;; Каждый раздел соответствует слайду, с комментариями и исполняемыми примерами.
;; Для запуска: Загрузите в SBCL или другой Common Lisp интерпретатор и выполните (load "examples.lisp")

;;; Слайд: Типы данных: примитивные
;; Примеры создания и использования примитивных типов

;; Числа (Number)
(defvar my-integer 42)  ; Целое число
(defvar my-float 3.14)  ; Число с плавающей точкой
(defvar my-ratio 1/2)   ; Рациональная дробь

(print my-integer)      ; Вывод: 42
(print my-float)        ; Вывод: 3.14
(print my-ratio)        ; Вывод: 1/2

;; Символ (Symbol)
(defvar my-symbol 'hello)  ; Символ как идентификатор

(print my-symbol)          ; Вывод: HELLO (символы обычно в верхнем регистре)

;; Строка (String)
(defvar my-string "Hello, Lisp!")

(print my-string)          ; Вывод: "Hello, Lisp!"

;; Булевы значения (Boolean)
(defvar true-value t)      ; Истинное значение
(defvar false-value nil)   ; Ложное значение (также пустой список)

(print true-value)         ; Вывод: T
(print false-value)        ; Вывод: NIL

;; Cons-ячейка (Cons Cell)
(defvar my-cons (cons 1 2))  ; Создание cons-ячейки: car=1, cdr=2

(print (car my-cons))        ; Вывод: 1 (первый элемент)
(print (cdr my-cons))        ; Вывод: 2 (второй элемент)

;;; Слайд: Типы данных: пользовательские
;; Примеры создания структур и классов

;; Структуры (Struct)
(defstruct person
  name
  age)

(defvar ivan (make-person :name "Ivan" :age 30))  ; Создание экземпляра структуры

(print (person-name ivan))                        ; Вывод: "Ivan"
(print (person-age ivan))                         ; Вывод: 30

;; Классы (CLOS)
(defclass animal ()
  ((species :initarg :species :accessor species)
   (sound :initarg :sound :accessor sound)))

(defvar cow (make-instance 'animal :species "Cow" :sound "Muuu"))

(print (species cow))  ; Вывод: "Cow"
(print (sound cow))    ; Вывод: "Muuu"

;;; Слайд: Операции над типами
;; Примеры использования type-of и typep

(defvar test-obj 100)

(print (type-of test-obj))    ; Вывод: INTEGER (тип объекта)

(print (typep test-obj 'integer))  ; Вывод: T (проверка принадлежности типу)
(print (typep test-obj 'string))   ; Вывод: NIL

;;; Слайд: Преобразование типов
;; Примеры преобразования типов

;; coerce
(print (coerce 3.8 'integer))  ; Вывод: 3 (преобразование float в integer)

;; parse-integer
(print (parse-integer "52"))   ; Вывод: 52 (строка в целое число)

;; format для строки
(print (format nil "~A" 52))   ; Вывод: "52" (число в строку)

;; float
(print (float 5))              ; Вывод: 5.0 (integer в float)

;; character
(print (character "A"))        ; Вывод: #\A (строка в символ)

;;; Слайд: Сравнение значений и типов
;; Примеры функций сравнения

;; eq (один и тот же объект в памяти)
(defvar x 5)
(defvar y x)  ; y ссылается на тот же объект
(print (eq x y))  ; Вывод: T

(defvar z 5)  ; Новый объект с тем же значением
(print (eq x z))  ; Вывод: NIL (для чисел eq может быть непредсказуемым, лучше eql)

;; eql (для чисел, символов или одинаковых объектов)
(print (eql 5 5))     ; Вывод: T
(print (eql 5 5.0))   ; Вывод: NIL (разные типы)

;; equal (структурное равенство)
(print (equal '(1 2) '(1 2)))  ; Вывод: T (списки равны)
(print (equal "abc" "abc"))    ; Вывод: T (строки равны)

;; equalp (мягкое равенство, игнорирует регистр и типы чисел)
(print (equalp 5 5.0))         ; Вывод: T
(print (equalp "Abc" "abc"))   ; Вывод: T (игнорирует регистр)

;; Завершение файла
(format t "~%Все примеры завершены.~%") 
 
