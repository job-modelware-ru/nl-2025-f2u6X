;; examples-complexity.lisp
;; Примеры программ на Common Lisp для слайдов презентации "5_Lisp_управление_сложностью_кода_и_обобщения.pptx"
;; Для запуска: Загрузите в SBCL через (load "examples-complexity.lisp") или используйте Docker
;; Примечание: Для демонстрации ASDF и Quicklisp используйте REPL или терминал, так как они требуют внешней настройки.
;; Слайды: Система пакетов, Пространства имен и экспорт, Обобщенные функции, Системы – ASDF, Менеджер библиотек – Quicklisp, Введение в ООП

;;; Слайд: Система пакетов и пространства имен
;; Пример определения пакета и использования

(defpackage :my-package
  (:use :common-lisp)
  (:export :my-function :my-variable))

(in-package :my-package)

(defvar my-variable 42)

(defun my-function (x)
  (* x 2))

(print my-variable)  ; Вывод: 42
(print (my-function 5))  ; Вывод: 10

(in-package :cl-user)

(use-package :my-package)

(print my-variable)  ; Вывод: 42
(print (my-function 5))  ; Вывод: 10

;;; Слайд: Обобщенные функции
;; Пример defgeneric и defmethod с множественной диспетчеризацией

(defgeneric area (shape &key &allow-other-keys)
  (:documentation "Обобщенная функция для вычисления площади фигуры"))

(defmethod area ((shape (eql :circle)) &key (radius 0))
  (* pi radius radius))  ; Метод для круга

(defmethod area ((shape (eql :rectangle)) &key (length 0) (width 0))
  (* length width))  ; Метод для прямоугольника

(print (area :circle :radius 5))  ; Вывод: примерно 78.5398
(print (area :rectangle :length 4 :width 6))  ; Вывод: 24

;;; Слайд: Системы – ASDF
(in-package :my-system)

(defun hello ()
  (format t "Hello from my-system!~%"))

;;; Слайд: Менеджер библиотек – Quicklisp
;; Quicklisp – для установки и загрузки библиотек. Установите Quicklisp (см. quicklisp.org).
;; Примеры в REPL (не выполняйте в скрипте, так как Quicklisp требует установки):

;; (ql:quickload "cl-ppcre")
;; (print "Quicklisp loaded cl-ppcre")

;; (ql:system-apropos "ppcre")
;; (print "Search results for 'ppcre'")

;; (ql:update-all-dists)
;; (print "Updated Quicklisp distributions")

(format t "~%Quicklisp examples: Run in REPL after installing Quicklisp (see quicklisp.org).~%")

;;; Слайд: Введение в ООП
;; Пример CLOS с классами, методами и комбинациями методов (:before, :after, :around)

(defclass shape ()
  ((color :initarg :color :accessor shape-color :initform "red")))

(defclass circle (shape)
  ((radius :initarg :radius :accessor circle-radius)))

(defgeneric draw (shape)
  (:documentation "Обобщенная функция для рисования фигуры"))

(defmethod draw ((s shape))
  (print (format nil "Drawing shape with color ~a" (shape-color s))))

(defmethod draw :before ((s circle))
  (print (format nil "Before drawing circle with radius ~a" (circle-radius s))))

(defmethod draw :after ((s circle))
  (print "After drawing circle"))

(defmethod draw :around ((s circle))
  (print "Around drawing circle")
  (call-next-method)
  (print "End around drawing circle"))

(let ((c (make-instance 'circle :radius 5 :color "blue")))
  (draw c))  ; Вывод:
             ; "Around drawing circle"
             ; "Before drawing circle with radius 5"
             ; "Drawing shape with color blue"
             ; "After drawing circle"
             ; "End around drawing circle"

;; Завершение файла
(format t "~%Все примеры завершены.~%")