(define (square x) (* x x))

(define (improve guess x)
 (/ (+ (/ x (square guess)) (* 2 guess))
  3))

(define (good-enought? guess last-guess)
 (< (/ (abs (- last-guess guess)) last-guess) 0.001))

(define (sqrt-iter guess last-guess x)
 (if (good-enought? guess last-guess)
     guess
     (sqrt-iter (improve guess x) guess x)))

(define (sqrt x)
 (sqrt-iter 1.0 0 x))

(sqrt 9)
