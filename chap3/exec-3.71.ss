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

(define integers (integer-starting-from 1))

(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
        (stream-car s1)
        (interleave s2 (stream-cdr s1)))))

(define (merge-weighted s1 s2 weight)
  (if (stream-null? s1) 
      s2
      (let ((p1 (stream-car s1))
            (p2 (stream-car s2)))
        ;(display (list 'mw p1 p2))(newline)
        (cond ((< (weight p1) (weight p2))
               (cons-stream 
                 p1
                 (merge-weighted (stream-cdr s1) s2 weight)))
              ((>= (weight p1) (weight p2))
               (cons-stream
                 p2
                 (merge-weighted s1 (stream-cdr s2) weight)))))))
              ;(else
              ; (cons-stream
              ;   p1
              ;   (merge-weighted (stream-cdr s1)
              ;                   (stream-cdr s2)
              ;                   weight)))))))

(define (pairs s t weight)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define order-proc
    (lambda (p)
      (let ((i (car p)) (j (cadr p)))
        (+ (* i i i) (* j j j)))))

(define ordered-pairs
  (pairs 
    integers
    integers
    order-proc))

(define (find-conse-pairs s count)
  (define (iter ns last index)
   (cond ((= index count) 'c-done)
         ((stream-null? ns) 'done)
         ((not (eq? (order-proc last) 
                    (order-proc (stream-car ns))))
          (iter (stream-cdr ns) (stream-car ns) index))
         (else
          (display (list (stream-car ns) last (order-proc last)))(newline) 
          (iter (stream-cdr ns) (stream-car ns) (1+ index)))))
  (iter (stream-cdr s) (stream-car s) 0))

(find-conse-pairs ordered-pairs 5)

; 
