;; io-demo.scm — стандартный ввод-вывод: файлы, порты

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

(define filename "demo.txt")

;; Запись файла
(call-with-output-file filename
  (lambda (port)
    (display "line1\n" port)
    (display "line2\n" port)))

;; Чтение файла целиком (простым способом)
(define content
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port 'concat)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(show "read lines" content)


