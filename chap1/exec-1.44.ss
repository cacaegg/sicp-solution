(define (compose f g)
 (lambda (x) (f (g x))))

(define (repeated f x)
 (define (iter n result)
  (if (< n 2)
      result
      (iter (1- n)
            (compose f result))))
 (iter x f))

(define dx 0.00001)
(define (smooth f)
 (lambda (x) (/ (+ (f (- x dx)) 
                   (f x)
                   (f (+ x dx)))
                3.0)))
(define (n-smooth f n)
 ((repeated smooth n) f))

(define (square x) (* x x))

(define (fx x) (1+ (square x)))
((n-smooth fx 1) 5)
