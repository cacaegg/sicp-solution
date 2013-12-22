(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin 
                 (set! result (proc))
                 (set! already-run? #t)
                 result)
          (begin
                 ;(display (list "got memory:" result))(newline)
                 result)))))
;(define (delay-m e) 
;  (memo-proc (lambda () e)))
; (define (delay e) (lambda () e))
; (define-syntax (delay

(define the-empty-stream '())
;(define (cons-stream a b)
;  (cons a (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (memo-proc (delay b))))))
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
  ;(display stream (pred (stream-car stream)))(newline)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))
(define (mul-stream s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (mul-stream (stream-cdr s1) (stream-cdr s2))))
(define (scale-stream stream factor)
  (cons-stream (* (stream-car stream) factor)
               (scale-stream (stream-cdr stream) factor)))
(define (stream-limit s tolerance)
  (let ((s0 (stream-car s))
        (s1 (stream-cdr s)))
    (if (> tolerance
           (abs (- s0 (stream-car s1))))
        (cons (stream-car s1) (lambda () the-empty-stream))
        (cons-stream s0
                     (stream-limit s1 tolerance)))))
(define (show x)
  (display x)(newline)
  x)
(define (display-line x)
  (newline)
  (display x))
(define (display-stream s n)
  (cond ((= n -1) (stream-for-each display-line s))
        ((= n 0) 
         (display (stream-car s))(newline)
         'done)
        (else
          (display (stream-car s))(newline)
          (display-stream (stream-cdr s) (1- n)))))

(define ones 
  (cons-stream 1 ones))

(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(display-stream (stream-limit (sqrt-stream 3132818271) 0.000000000000000000001) -1)
