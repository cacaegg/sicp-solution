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
  ; (display (list 'stream-map proc argstreams))(newline)
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

(define integers (integer-starting-from 1))

(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                (if (stream-null? integrand)
                    the-empty-stream
                    (integral (delay (stream-cdr integrand))
                              (+ (* dt (stream-car integrand))
                                 initial-value)
                              dt)))))


(define (square x) (* x x))

(define (random-in-range low high)
 (let ((range (- high low)))
  (+ (random range) low)))

(define (estimate-integral P? x1 x2 y1 y2)
 (let ((in-cir? (P? x1 x2 y1 y2)))
  (define (integral-stream x1 x2 y1 y2)
    (cons-stream 
      (in-cir? (random-in-range x1 x2) (random-in-range y1 y2))
      (integral-stream x1 x2 y1 y2)))
  (define estimate-stream 
    (stream-map
      (lambda (x) (* (* (abs (- x2 x1)) (abs (- y2 y1))) x))
      (monte-carlo (delay (integral-stream x1 x2 y1 y2)) 0 0)))
  estimate-stream))

(define (in-circle? x1 x2 y1 y2)
 (let ((center-x (/ (+ x1 x2) 2))
       (center-y (/ (+ y1 y2) 2))
       (r (/ (- x2 x1) 2)))
  ;(display (list "in-circle?" x1 x2 y1 y2 center-x center-y r))(newline)
  (lambda (x y)
   ;(display (list "in-lambda:" x y))(newline)
   (<= 
    (+ (square (abs (- x center-x)))
       (square (abs (- y center-y))))
    (square r)))))

(define (monte-carlo experiment-stream passed failed)
 (define (next passed failed)
   ; (display (list 'inner (force experiment-stream) passed failed))(newline)
   (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
       (delay (stream-cdr (force experiment-stream))) passed failed)))
 ; (display (list 'outer (force experiment-stream) passed failed))(newline)
 (if (stream-car (force experiment-stream))
     (next (1+ passed) failed)
     (next passed (1+ failed))))

(display-stream (estimate-integral in-circle? -1.0 1.0 -1.0 1.0) 16384)
