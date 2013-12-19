(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-stream (partial-sums s)
                           (stream-cdr s))))
