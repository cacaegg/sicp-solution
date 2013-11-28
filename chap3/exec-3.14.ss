(define (mystery x)
 (define (loop x y)
  (if (null? x)
      y
      (let ((temp (cdr x)))
       (set-cdr! x y)
       (loop temp x))))
 (loop x '()))
; mystery is a revese function of a list

(define v (list 'a 'b 'c 'd))
; Box-and-Pointer Diagram of v
; |---|---|    |---|---|    |---|---|    |---|---|
; | | | ------>| | | ------>| | | ------>| | | X |
; |_|_|___|    |_|_|___|    |_|_|___|    |_|_|___|
;   |            |            |            |
;   v            v            v            v
;   a            b            c            d
;
; (a b c d)  
v


(define w (mystery v))
; Box-and-Pointer Diagram of w
; |---|---|    |---|---|    |---|---|    |---|---|
; | | | ------>| | | ------>| | | ------>| | | X |
; |_|_|___|    |_|_|___|    |_|_|___|    |_|_|___|
;   |            |            |            |
;   v            v            v            v
;   d            c            b            a
;
; (d c b a)
w

; Please note that after (mystery v)
; The evaluation of v -> (x)
v
