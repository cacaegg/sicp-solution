(define (smallest-divisor n)
 (find-divisor n 2))

(define (next x)
 (if (= x 2)
     3
     (+ x 2)))

(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
       ((divides? n test-divisor) test-divisor)
       (else (find-divisor n (next test-divisor)))))

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

; 1. No, it's not twice faster.
; 2. From 11 cpu-time to 35 cpu-time -> 3.142...
; 3. Two algorithm's ratio should roughly be 1:1.3
;    This might due to the prime is found before (/ (sqrt n) 2), 
;    so that the twice will not meet.
