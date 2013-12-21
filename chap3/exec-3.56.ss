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
(define (scale-stream stream factor)
  (cons-stream (* (stream-car stream) factor)
               (scale-stream (stream-cdr stream) factor)))
(define (show x)
  (display x)(newline)
  x)
(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge s2 (merge s3 s5))))
(define s2 (scale-stream S 2))
(define s3 (scale-stream S 3))
(define s5 (scale-stream S 5))
(stream-ref S 3)
