(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; (expand 1 7 10)
; > (1 (expand (remainder (* 1 10) 7) 7 10))
; > (1 4 (expand (remainder (* 3 10) 7) 7 10))
; > (1 4 2 (expand (remainder (* 2 10) 7) 7 10))
; > (1 4 2 8 (expand (remainder (* 6 10) 7) 7 10))
; ...
;
; (expand 3 8 10)
; > (3 (expand (remainder (* 3 10) 8) 8 10))
; > (3 7 (expand (remainder (* 6 10) 8) 8 10))
; > (3 7 5 (expand (remainder (* 4 10) 8) 8 10))
; > (3 7 5 0 (expand (remainder (* 0 10) 8) 8 10))
; > (3 7 5 0 0 (expand (remainder (* 0 10) 8) 8 10))
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
(define (show x)
  (display x)(newline)
  x)
(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))
