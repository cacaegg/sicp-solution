; a.
(define inverse-stream
  (cons-stream 1
               (stream-map  / ones (integer-starting-from 2))))

(define (integrate-series s)
 (mul-stream s
             inverse-stream))

; b.
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 
               (integrate-series 
                 (stream-map (lambda (x) (* -1 x))
                             sine-series))))
(define sine-series
  (cons-stream 0
               (integrate-series consie-series)))
