;; closures-demo.scm — замыкания: функции “помнят” окружение

(define (show label v)
  (display label)
  (display ": ")
  (write v)
  (newline))

;; make-counter возвращает функцию, которая хранит состояние count
(define (make-counter start)
  (let ((count start))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define c1 (make-counter 0))
(define c2 (make-counter 100))

(show "c1()" (c1))
(show "c1()" (c1))
(show "c2()" (c2))
(show "c1()" (c1))
(show "c2()" (c2))

;; Ещё пример: “приватное поле” через замыкание (мини-объект)
(define (make-account balance0)
  (let ((balance balance0))
    (lambda (msg . args)
      (cond
        ((eq? msg 'balance) balance)
        ((eq? msg 'deposit)
         (let ((amount (car args)))
           (set! balance (+ balance amount))
           balance))
        ((eq? msg 'withdraw)
         (let ((amount (car args)))
           (if (> amount balance)
               (error "Insufficient funds" amount balance)
               (begin
                 (set! balance (- balance amount))
                 balance))))
        (else (error "Unknown message" msg))))))

(define acc (make-account 50))
(show "balance" (acc 'balance))
(show "deposit 20" (acc 'deposit 20))
(show "withdraw 10" (acc 'withdraw 10))


