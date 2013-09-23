(define (square x) (* x x))

(define (good-enought? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
 (average guess (/ x guess)))

(define (average x y)
 (/ (+ x y) 2))

(define (sqrt-iter guess x)
 (if (good-enought? guess x)
     guess
     (sqrt-iter (improve guess x) x)))

(define (sqrt x)
 (sqrt-iter 1.0 x))

; Very small number
(sqrt 0.00000001)
; > 0.03125010656242753
; The answer can good enough when number of root of square is less than 0.001
; Because the good-enought? will always think such number is good enough

; Very large number
; (sqrt 100000000000000000000000000000000000.0)
; For very large number, the floating point is not precise enought to represent delta
; Hence, the good enought will never return #t

; New implementation, check the fraction of change between guesses is less than 0.001,
(define (good-enought? guess last-guess)
 (< (/ (abs (- last-guess guess)) last-guess) 0.001))

(define (sqrt-iter guess last-guess x)
 (if (good-enought? guess last-guess)
     guess
     (sqrt-iter (improve guess x) guess x)))

(define (sqrt x)
 (sqrt-iter 1.0 0.0 x))

(sqrt 0.00000001)
(sqrt (square 1000000000000000.0))
