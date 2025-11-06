(format t "~%=== 1. Стандартная библиотека ввода-вывода ===~%")
(with-open-file (out "/output.txt" :direction :output :if-exists :supersede)
  (format out "Привет, Lisp!~%Число: ~d, Строка: ~s, Плавающее: ~f~%" 42 "hello" 3.14159))
  
(with-open-file (in "/output.txt")
  (loop for line = (read-line in nil nil)
        while line
        do (format t "  ~a~%" line)))

(format t "~{~a~^, ~}~%" '(яблоко банан груша))

(format t "~% 2. Управление памятью~%")

;; 1. Принудительный сбор мусора
(format t "Вызов сборщика мусора...~%")
(sb-ext:gc :full t)
(format t "Динамическая куча: ~:d байт~%" (sb-vm::dynamic-usage))

;; 2. Создаём большой объект
(let ((big-array (make-array 1000000 :element-type 'character :initial-element #\A)))
  (format t "Создан массив: ~d элементов~%" (length big-array)))
  (format t "Динамическая куча: ~:d байт~%" (sb-vm::dynamic-usage))

;; 3. GC — память освобождается
(sb-ext:gc :full t)
(format t "Сборщик мусора выполнен — память освобождена~%")
(format t "Динамическая куча: ~:d байт~%" (sb-vm::dynamic-usage))


;; 5. Сохранение образа
(format t "~%Сохранение образа (не в Docker):~%")
(format t "  (save-lisp-and-die \"my-app.core\")~%")

(format t "~% 3. Конкурентность (bordeaux-threads)~%")
(handler-case
    (progn
      ;; Загружаем bordeaux-threads
      (ql:quickload "bordeaux-threads" :silent t)

      ;; Импортируем нужные символы с shadowing-import
      (shadowing-import '(bordeaux-threads:make-thread
                          bordeaux-threads:join-thread
                          bordeaux-threads:make-lock
                          bordeaux-threads:with-lock-held
                          bordeaux-threads:timeout))

      ;; Переменные
      (defvar *counter* 0)
      (defvar *lock* (make-lock "counter-lock"))

      ;; Функция
      (defun worker ()
        (dotimes (i 100000)
          (with-lock-held (*lock*)
            (incf *counter*)))

      ;; Запускаем потоки
      (let ((t1 (make-thread #'worker))
            (t2 (make-thread #'worker)))
        (join-thread t1)
        (join-thread t2))

      (format t "  Итоговый счётчик: ~d (ожидается 200000)~%" *counter*))
  (error (e)
    (format t "  Ошибка в потоках: ~a~%" e))))

(format t "~% 4. Макросы ~%")
(defmacro while (test &body body)
  `(loop while ,test do (progn ,@body)))
(let ((i 0)) (while (< i 3) (format t "i=~d~%" i) (incf i)))

(format t "~% 5. Функциональное программирование~%")
(defparameter *nums* '(1 2 3 4 5 6 7 8 9 10))
(format t "Квадраты: ~a~%" (mapcar (lambda (x) (* x x)) *nums*))
(format t "Сумма: ~a~%" (reduce #'+ *nums*))
(format t "Чётные: ~a~%" (remove-if-not #'evenp *nums*))