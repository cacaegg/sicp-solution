(define (i-cons a b)
 (* (expt 2 a) (expt 3 b)))

(define (i-car p)
 (define (de-factor p)
  (if (= (mod p 3) 0)
      (de-factor (/ p 3))
      p))
 (log (de-factor p) 2))


(define (i-cdr p)
 (define (de-factor p)
  (if (= (mod p 2) 0)
      (de-factor (/ p 2))
      p))
 (log (de-factor p) 3))

(let ((x (i-cons 2 2)))
 x)

(let ((x (i-cons 2 3)))
 (i-car x))

(let ((x (i-cons 2 3)))
 (i-cdr x))
