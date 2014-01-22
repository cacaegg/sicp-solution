(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (amb (+ low 1) high)))
