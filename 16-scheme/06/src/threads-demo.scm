;; threads-demo.scm — конкурентность: потоки (threads) в Guile
;;
;; Важно: это про конкурентность (concurrency), не обязательно про параллелизм (parallelism).
;; Реальная параллельность зависит от реализации/VM/настроек.

(use-modules (ice-9 threads))

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

(define (work name n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        (begin
          (display name)
          (display " done, acc=")
          (display acc)
          (newline)
          acc)
        (loop (+ i 1) (+ acc i)))))

(define t1 (make-thread (lambda () (work "t1" 100000)) "t1"))
(define t2 (make-thread (lambda () (work "t2" 120000)) "t2"))

(thread-start! t1)
(thread-start! t2)

(define r1 (join-thread t1))
(define r2 (join-thread t2))

(show "joined r1" r1)
(show "joined r2" r2)


