(define (square x) (* x x))

(define (fast-prime? n times)
 (cond ((= times 0) #t)
       ((fermat-test n) (fast-prime? n (- times 1)))
       (else #f)))

(define (fermat-test n x)
 (define (try-it a)
  (= (expmod a n n) a))
 (try-it x))

(define (expmod base exp m)
 (define (n-tri-check x)
  (define (inner-check x root)
   (if (and (not (= x 1)) (not (= x (- m 1))) (= root 1))
       0
       root))
  (inner-check x (remainder (square x) m)))
 (cond ((= exp 0) 1)
       ((even? exp) 
        (n-tri-check (expmod base (/ exp 2) m)))
       (else
        (remainder (* base (expmod base (- exp 1) m))
                   m))))

(define (test-fermat n)
 (define (test-iter x) 
  (cond ((= x 1) #t)
        ((not (fermat-test n x)) x)
        (else (test-iter (- x 1)))))
 (test-iter (- n 1)))

(test-fermat 561)
(test-fermat 1105)
(test-fermat 1729)
(test-fermat 2465)
(test-fermat 2821)
(test-fermat 6601)
