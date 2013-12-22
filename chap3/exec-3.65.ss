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


(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))


(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
      (- s2
         (/ (square (- s2 s1))
            (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform ln2-stream) 16)

;(display-stream (euler-transform pi-stream) 4096)
