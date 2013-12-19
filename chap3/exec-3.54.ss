(define (mul-stream s s)
  (stream-map * s s))

(define factorials
  (cons-stream 1
               (mul-stream (add-stream one integers)
                           factorials)))

; (1 2 6 24 120 ...)
