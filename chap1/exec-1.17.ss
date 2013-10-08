(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (* a b)
 (cond ((= b 1) a)
       ((even? b) (double (* a (halve b))))
       (else (+ a (* a (- b 1))))))

(* 3 5)
(* 12 12)
(* 15 100)
