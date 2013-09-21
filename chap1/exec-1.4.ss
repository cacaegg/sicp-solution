;   When interpreter is trying to evaluate combination ((if (> b 0) + -) a b)
; It needs to evaluate operator (if (> b 0) + -) first. So if b is greater than
; zero, then sub-expression of operator will return + procedure, - otherwise.
;   Then interpreter will apply paramter a b to the  result of combination of 
; operator (+ or -). Hence, it will be (+ a b) if b is grater than zero, or
; (- a b) otherwise.
(define (a-plus-abs-b a b)
 ((if (> b 0) + -) a b))

(a-plus-abs-b 1 1) ; 2
(a-plus-abs-b 1 0) ; 1
(a-plus-abs-b 1 -1) ; 2
