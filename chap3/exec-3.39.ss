; (define x 10)
; (define s (make-serializer))
; (parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) -> f
;                   (s (lambda () (set! x (+ x 1))))) -> s
;
; f-read, s, f-set > 100
; s, f-read, f-set > 121
; f-read, f-set, s > 101
