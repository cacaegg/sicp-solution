(define (average a b) (/ (+ a b) 2))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (average-point a b)
  (let ((mid-x (average (x-point a) (x-point b)))
        (mid-y (average (y-point a) (y-point b))))
   (make-point mid-x mid-y)))
    
(define (midpoint-segment seg)
 (let ((s (start-segment seg)) (e (end-segment seg)))
  (average-point s e)))

(define (print-point p)
 (newline)
 (display "(")
 (display (x-point p))
 (display ",")
 (display (y-point p))
 (display ")"))

(let ((x (make-point 3 6)) (y (make-point 4 8)))
 (print-point (midpoint-segment (make-segment x y))))


