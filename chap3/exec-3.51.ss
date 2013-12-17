(define the-empty-stream '())
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? stream) (eq? the-empty-stream stream))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (1- n))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (1+ low) high))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))
(define (show x)
  (display x)(newline)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
; 0 
; 1 
; 2 
; 3 
; 4 
; 5 
; 6 
; 7
; 8
; 9
; 10

(stream-ref x 5)
; 5

(stream-ref x 7)
; 7
