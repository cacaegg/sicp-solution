(define (smooth s)
  (define (extracted-stream stream last)
    (cons-stream
      (/ (+ last (stream-car stream)) 2)
      (extracted-stream (stream-cdr stream)
                        (stream-car stream))))
  (extracted-stream (stream-cdr s) 
                    (stream-car s)))
(define (make-zero-crossing input-stream last-val)
  (let ((ext-stream (smooth input-stream)))
    (stream-map sign-change-detector
                ext-stream
                (cons-stream 0 ext-stream))))
