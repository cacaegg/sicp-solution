(define (for-each proc items)
  (if (null? items)
      (quote done)
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; a. It can be done without problem due to all the argument
; will be forced again before it is applied to primitive procedure
;

(define (p1 x)
  (set! x (cons x (quote (2))))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x (quote (2))))))

; b.
; Original version
; (p1 1)
; > (1 2)
; (p2 1)
; > 1
;
; Cy D. Fect version
; (p1 1)
; > (1 2)
; (p2 1)
; (1 2)

; c. Since the difference between Cy D. Fect's version and original version
;    is time of getting the value. (The value will be the same as long as the 
;    environment has been reserved.)

; d. I prefer Cy's version due to it handle much more correctly when there is 
;    side effects.
