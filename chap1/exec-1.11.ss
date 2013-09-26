; Recursive
(define (f-rec n)
 (if (< n 3)
     n
     (+ (f-rec (- n 1))
        (* 2 (f-rec (- n 2)))
        (* 3 (f-rec (- n 3))))))
(f-rec 4)
(f-rec 10)
(f-rec 28)

; Iterative
(define (f-iter n)
 (f-loop 2 1 0 n))

(define (f-loop x y z count)
 (if (= count 0)
     z
     (f-loop (+ x (* 2 y) (* 3 z)) 
             x
             y
             (- count 1))))

(f-iter 4)
(f-iter 10)
(f-iter 28)
