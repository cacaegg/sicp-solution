(define (last-pair x)
 (if (null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
 (set-cdr! (last-pair x) x)
 x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cyclic? ls)
 (define (in-list? x pair-list)
  (cond ((null? pair-list) #f)
        ((eq? x (car pair-list)) #t)
        (else (in-list? x (cdr pair-list)))))
 (define (count-iter x counted-pairs)
  (cond ((not (pair? x)) #f)
        ((in-list? x counted-pairs) #t)
        (else
         (count-iter 
          (cdr x) 
          (cons x counted-pairs)))))
 (count-iter ls '()))
(cyclic? z)
(cyclic? (list 1 2 (list 4 5)))

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
