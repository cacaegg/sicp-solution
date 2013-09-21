; Due to the Procedure is trying to evaluate itself, once it is evaluted, 
; process will drops into infinite loop
(define (p) (p))

; So test procedure should return zero if x is zero, y otherwise
(define (test x y)
 (if (= x 0)
     0
     y))

; In case of applicative-order evaluation, (p) will be evaluated first.
; Hence, the process never return when it encounters applicative-order 
; evaluation interpreter. However, this combination can return zero if
; it is a normal-order evaluation intepreter
(test 0 (p))
