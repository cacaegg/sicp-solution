(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high))
          (ksq (+ (* i i) (* j j))))
      (require (>= hsq ksq))
      (let ((k (sqrt ksq)))
        (require (integer? k))
        (list i j k)))))

;; Yes, this solution test only for k that is i*i + j*j 
;; As a result, this can reduce much more possible values
;; need to be test.
