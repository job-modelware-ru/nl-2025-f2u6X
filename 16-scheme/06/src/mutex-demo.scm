;; mutex-demo.scm — shared mutable state + mutex в Guile

(use-modules (ice-9 threads))

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

(define counter 0)
(define m (make-mutex))

(define (inc-n n)
  (let loop ((i 0))
    (if (= i n)
        #t
        (begin
          (lock-mutex m)
          (set! counter (+ counter 1))
          (unlock-mutex m)
          (loop (+ i 1))))))

(define t1 (make-thread (lambda () (inc-n 50000)) "t1"))
(define t2 (make-thread (lambda () (inc-n 50000)) "t2"))

(thread-start! t1)
(thread-start! t2)
(join-thread t1)
(join-thread t2)

(show "counter (expected 100000)" counter)


