(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (display n)(newline)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)

; This will always try to evaluate the argument of unless
; Hence, this programm will not terminated
