(define (compose f g)
 (lambda (x) (f (g x))))

(define (repeated f x)
 (define (iter n result)
  (if (< n 2)
      result
      (iter (1- n)
            (compose f result))))
 (iter x f))

(define (square x) (* x x))
((repeated square 2) 5)
