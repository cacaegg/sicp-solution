(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; (expand 1 7 10)
; > (1 (expand (remainder (* 1 10) 7) 7 10))
; > (1 4 (expand (remainder (* 3 10) 7) 7 10))
; > (1 4 2 (expand (remainder (* 2 10) 7) 7 10))
; > (1 4 2 8 (expand (remainder (* 6 10) 7) 7 10))
; ...
;
; (expand 3 8 10)
; > (3 (expand (remainder (* 3 10) 8) 8 10))
; > (3 7 (expand (remainder (* 6 10) 8) 8 10))
; > (3 7 5 (expand (remainder (* 4 10) 8) 8 10))
; > (3 7 5 0 (expand (remainder (* 0 10) 8) 8 10))
; > (3 7 5 0 0 (expand (remainder (* 0 10) 8) 8 10))
