(define (make-accumulator init)
 (lambda (i)
  (set! init (+ init i))
  init))

(define accu_1 (make-accumulator 10))
(define accu_2 (make-accumulator 12))
(accu_1 5)
(accu_2 5)

(accu_1 1)
(accu_2 1)
