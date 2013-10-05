(define (square x) (* x x))

(define (fast-prime? n times)
 (cond ((= times 0) #t)
       ((fermat-test n) (fast-prime? n (- times 1)))
       (else #f)))

(define (fermat-test n)
 (define (try-it a)
  (= (expmod a n n) a))
 (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
 (remainder (fast-expt base exp) m))

(define (fast-expt b e)
 (cond ((= e 0) 1)
       ((even? e) (square (fast-expt b (/ e 2))))
       (else (* b (fast-expt b (- e 1))))))

(define (timed-prime-test n)
 (newline)
 (display n)
 (start-prime-test n (time-process)))

(define (start-prime-test n start-time)
 (if (fast-prime? n 3)
     (report-prime (- (cpu-time) start-time))
     ))

(define (report-prime elapsed-time)
 (display " *** ")
 (display elapsed-time)
 (newline)
)

(define (ranged-prime start end)
 (cond ((> start end) #f)
       ((even? start) (ranged-prime (+ start 1) end))
       ((even? end) (ranged-prime start (- end 1)))
       ((fast-prime? start 3) (start-prime-test start (cpu-time)) start)
       (else (ranged-prime (+ start 2) end))))

(ranged-prime 100000000 200000000)
(ranged-prime 100000008 200000000)
(ranged-prime 100000038 200000000)

(ranged-prime 1000000000 2000000000)
(ranged-prime 1000000008 2000000000)
(ranged-prime 1000000010 2000000000)

(ranged-prime 10000000000 20000000000)
(ranged-prime 10000000020 20000000000)
(ranged-prime 10000000034 20000000000)

(ranged-prime 100000000000 200000000000)
(ranged-prime 100000000004 200000000000)
(ranged-prime 100000000020 200000000000)

; 1. No, fast-prime will produce large number that need mounts of CPU time to compute.
; 2. Since it's applicative order interpreter, the remainder in expmod will reduce 
;    the number each time expmod, so the number will not grows too large.
