(define (new-if predicate then-clause else-clause)
 (cond (predicate then-clause)
       (else else-clause)))

(define (square x) (* x x))

(define (good-enought? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
 (average guess (/ x guess)))

(define (average x y)
 (/ (+ x y) 2))

(define (sqrt-iter guess x)
 (new-if (good-enought? guess x)
         guess
         (sqrt-iter (improve guess x) x)))

; Procedure will run out of memory. It's beacause of cond clauses are always
; evaluated. So the else claused will keep evluate itself in case of applicative
; order evaluation. 

(define (sqrt x)
 (sqrt-iter 1.0 x))

(sqrt 9)
