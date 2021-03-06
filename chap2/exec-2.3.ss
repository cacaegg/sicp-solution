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

; Implementation 1, use two points as represent of rectangle
(define (make-rec-1 p1 p2)
 (cons p1 p2))
(define (rec-length-1 r)
 (abs 
  (- (x-point (car r))
     (x-point (cdr r)))))
(define (rec-width-1 r)
 (abs 
  (- (y-point (car r))
     (y-point (cdr r)))))
(define (perimeter-1 r)
 (+ (* 2 (rec-length-1 r))
    (* 2 (rec-width-1 r))))
(define (rec-area-1 r)
 (* (rec-length-1 r) (rec-width-1 r)))

(let ((p1 (make-point 1 1)) (p2 (make-point 3 4)))
 (let ((r1 (make-rec-1 p1 p2)))
  (perimeter-1 r1)))
(let ((p1 (make-point 1 1)) (p2 (make-point 3 4)))
 (let ((r1 (make-rec-1 p1 p2)))
  (rec-area-1 r1)))

; Implementation 2, use a point, length & width as represent of rectangle
(define (make-rec-2 p1 p2)
 (let ((len (abs (- (x-point p1) (x-point p2))))
       (width (abs (- (y-point p1) (y-point p2)))))
  (cons p1 (cons len width))))
(define (rec-length-2 r)
 (car (cdr r)))
(define (rec-width-2 r)
 (cdr (cdr r)))
(define (perimeter-2 r)
 (+ (* 2 (rec-length-2 r))
    (* 2 (rec-width-2 r))))
(define (rec-area-2 r)
 (* (rec-length-2 r) (rec-width-2 r)))

(let ((p1 (make-point 1 1)) (p2 (make-point 3 4)))
 (let ((r1 (make-rec-2 p1 p2)))
  (perimeter-2 r1)))
(let ((p1 (make-point 1 1)) (p2 (make-point 3 4)))
 (let ((r1 (make-rec-2 p1 p2)))
  (rec-area-2 r1)))
