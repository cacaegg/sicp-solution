(define (attach-tag tag obj) (cons tag obj))
; New type & operation in Generic Operation
(define (newtype? z)
 (eq? (type-tag z) 'newtype))
(define (real-part-newtype z)
 (* newtype calculation on z))
(define (imag-part-newtype z)
 (/ newtype calculation on z))
; ... And so on for magnitude, angle, 
; make-from-real-imag-newtype, make-from-mag-ang-newtype
; Final, include this operation to interfaces
(define (real-part z)
 (cond ((rectangular? z)
        (real-part-rectangular (contents z)))
       ((polar? z)
        (real-part-polar (contents z)))
       ((newtype? z)
        (real-part-newtype (contents z)))
       (else (error real-part "Unknown type z -- REAL-PART" z))))

; Newtype & operation in data-directed style
(define (install-newtype)
 ; internal procedures
 (define (real-part z) (calculate z))
 (define (imag-part z) (calculate z))
 ; ...
 ;
 ; interfaces
 (define (tag x) (attach-tag 'newtype x))
 (put 'new-type 'real-part real-part)
 (put 'new-type 'imag-part imag-part)
 ; ...
 (put 'make-from-real-imag 'new-type
  (lambda (x y) (tag (make-from-real-imag x y))))
 (put 'make-from-mag-ang 'new-type
  (lambda (r a) (tag (make-from-mag-ang r a)))))

; Newtype & operation Message-passing style
(define (make-from-new-type z y)
 (lambda (op)
  (cond ((eq? op 'real-part) (calculate-real y z))
        ((eq? op 'imag-part) (calculate-imag y z))
        ; ...
        (else
         (error "Unknown op -- MAKE-FROM-NEW-TYPE" op)))))

; If there are many newtypes or operation often be added, both meesage-passing or data-directed
; is nice for it. Since message-passing only need to write a new-type constructor, and return 
; new-type data procedure. Message-passing style might be easier to maintain.
