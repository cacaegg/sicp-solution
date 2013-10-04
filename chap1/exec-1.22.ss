(define (smallest-divisor n)
 (find-divisor n 2))

(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
       ((divides? n test-divisor) test-divisor)
       (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
 (= (remainder a b) 0))

(define (square x) (* x x))

(define (prime? x)
 (if (= (smallest-divisor x) x) #t #f))

(define (timed-prime-test n)
 (newline)
 (display n)
 (start-prime-test n (time-process)))

(define (start-prime-test n start-time)
 (if (prime? n)
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
       ((prime? start) (start-prime-test start (cpu-time)) start)
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

; 1. Yes, it bear the result.
; 2. From 15 cpu-time to 46 cpu-time, (/ 46 15) -> 3.066666666666667
;    which is close to (sqrt 10)
; 3. Yes
