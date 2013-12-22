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

(define (integer-starting-from n)
  (cons-stream n
               (integer-starting-from (1+ n))))

(define inverse-stream
  (cons-stream 1
               (stream-map  / ones (integer-starting-from 2))))

(define (integrate-series s)
 (mul-stream s
             inverse-stream))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 
               (integrate-series 
                 (stream-map (lambda (x) (* -1 x))
                             sine-series))))
(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series
                 (scale-stream (stream-cdr s) -1)
                 (invert-unit-series s))))

(define (divide-series s1 s2)
  (let ((a0 (stream-car s2)))
    (if (= a0 0)
        (error 'divde-series "Divide by Zero" s1 s2)
        (mul-series
          s1
          (invert-unit-series (scale-stream s2 a0))))))
(define tangent-series
    (divide-series sine-series cosine-series))

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

(define (sqrt-stream-l x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream-l x))))
(define cs (sqrt-stream-l 2))

; Louis' version just call non-local state version sqrt-stream-l repeatly,
; hence the memo-proc will be drops out after every time.
; In contract, the original version, it return a procedure with local state,
; and call it self repeatly. Hence, the memo-proc cached result can be bind
; within local frame.

; Even we take of memo-proc in delay, the original version is still more efficient.
; It's due to it avoid to create a frame by call itself. But the Louis' version will
; use (sqrt-stream-l) in stream-map, this will result to create additional frame 
; each time stream-cdr is called.

(display-stream cs 5)
