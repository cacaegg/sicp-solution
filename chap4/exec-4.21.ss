#lang racket
; a
((lambda (n)
   ((lambda (fab)
      (fab fab n))
    (lambda (fb k)
      (cond ((or (= k 0) (= k 1)) 1)
            (else
             (+ (fb fb (- k 1))
                (fb fb (- k 2))))))))
 5)

; b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))
(f 1)
