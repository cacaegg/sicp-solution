(define (append x y)
 (if (null? x)
     y
     (cons (car x) (append (cdr x) y))))

(define (append! x y)
 (set-cdr! (last-pair x) y)
 x)

(define (last-pair x)
 (if (null? (cdr x))
     x
     (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
; x after append
; |---|---|    |---|---|
; | | | ------>| | | X |
; |_|_|___|    |_|_|___|
;   |            |
;   v            v
;   a            b
z
(cdr x)
; (b)

(define w (append! x y))
; x after append!
; |---|---|    |---|---|    |---|---|    |---|---|
; | | | ------>| | | ------>| | | ------>| | | X |
; |_|_|___|    |_|_|___|    |_|_|___|    |_|_|___|
;   |            |            |            |
;   v            v            v            v
;   a            b            c            d
w
(cdr x)
; (b c d)
