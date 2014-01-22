(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; It's due to some branch will terminate as soon as it find an answer.
; However, there might be other answer within that branch.
; 
; Hence, we can put an amb after list answer to get backtrack.

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)
        (amb)))))
