(define (last-pair x)
 (if (null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
 (set-cdr! (last-pair x) x)
 x)

(define z (make-cycle (list 'a 'b 'c 'd 'e 'f 'g)))

(define (cyclic? ls)
 (define (run turtle rabbit)
  (cond ((or (null? turtle)
             (null? rabbit))
         #f)
        ((or (not (pair? turtle))
             (not (pair? (cdr rabbit))))
         #f)
        ((eq? turtle rabbit) #t)
        (else
         (run (cdr turtle) (cdr (cdr rabbit))))))
 (if (pair? ls)
     (run ls (cdr ls))
     #f))
(cyclic? z)
(cyclic? (list 1))

;  The box-and-pointer diagram of cycle z
;    _______________________________
;    |                             |
;    v                             |
;  |---|---|    |---|---|    |---|-|-|
;  | | | -----> | | | -----> | | | | |
;  |_|_|___|    |_|_|___|    |_|_|___|
;    |            |            |
;    v            v            v
;    a            b            c
