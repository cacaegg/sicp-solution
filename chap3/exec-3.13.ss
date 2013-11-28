(define (last-pair x)
 (if (null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
 (set-cdr! (last-pair x) x)
 x)

(define z (make-cycle (list 'a 'b 'c)))
z

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
