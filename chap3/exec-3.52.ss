(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin 
                 (set! result (proc))
                 (set! already-run? #t)
                 result)
          (begin
                 (display (list "got memory:" result))(newline)
                 result)))))
(define (delay e) 
  (memo-proc (lambda () e)))
(define (delay-nom e) (lambda () e))

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
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))
(define (show x)
  (display x)(newline)
  x)
(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))

(define sum 0)
sum
; sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
sum
; sum = 0

(define seq 
  (stream-map accum (stream-enumerate-interval 1 20)))
(display "after stream-map")(newline)
seq
; sum = 210

(define y (stream-filter even? seq))
y
; X (1 . <proc>)
; (6 . #<procedure>)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
z
; X (1 . <proc>)
; (10 . <procedure>)
'(stream-ref y 7)
(stream-ref y 7)
; 136
'(display-stream z)
(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
;
; It's no different. But the second time on stream will run faster.
