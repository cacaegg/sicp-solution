(define (make-from-mag-ang m a)
 (lambda (op)
  (cond ((eq? op 'real-part) 
         (* m (cos a)))
        ((eq? op 'imag-part)
         (* m (sin a)))
        ((eq? op 'magnitude) m)
        ((eq? op 'angle) a)
        (else 
         (error make-from-mag-ang "Unknown op -- MAKE-FROM-MAG-ANG" op)))))
(define (apply-generic op arg) (arg op))

(let ((v (make-from-mag-ang 5 45)))
 (apply-generic 'real-part v))

(let ((v (make-from-mag-ang 5 45)))
 (apply-generic 'imag-part v))

(let ((v (make-from-mag-ang 5 45)))
 (apply-generic 'magnitude v))

(let ((v (make-from-mag-ang 5 45)))
 (apply-generic 'angle v))


