(define s (cons-stream 1 (add-streams s s)))

; First, s is a pair, car is 1, cdr is a promise to evaluate (add-stream s s)
; When run cdr, we evaluate (add-stream s s), which expand to (stream-map + s s)
; and it returns a pair come from 
; (cons-stream (apply + 1 1) (apply stream-map (cons + (map stream-cdr s s))))
; (2 (promise to do (stream-map + 2 2)))
; (4 (promise to do (stream-map + 4 4)))
; and so on...
