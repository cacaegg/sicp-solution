(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (width i)
 (/ (- (upper-bound i) (lower-bound i)) 2.0))
(define (sub-interval a b)
 (make-interval (- (lower-bound a) (lower-bound b))
                (- (upper-bound a) (upper-bound b))))
(define (add-interval a b)
 (make-interval (+ (lower-bound a) (lower-bound b))
                (+ (upper-bound a) (upper-bound b))))
(define (mul-interval x y)
 (let ((lx (lower-bound x)) (ux (upper-bound x))
       (ly (lower-bound y)) (uy (upper-bound y)))
  (cond ((and (>= lx 0) (>= ux 0) (>= ly 0) (>= uy 0))  ; x++ y++
         (make-interval (* lx ly) (* ux uy)))
        ((and (< lx 0) (>= ux 0)  (>= ly 0) (>= uy 0))
         (make-interval (* lx uy) (* ux uy)))           ; x+- y++
        ((and (< lx 0) (< ux 0) (>= ly 0) (>= uy 0))
         (make-interval (* lx uy) (* ly ux)))           ; x-- y++
        ((and (>= lx 0) (>= ux 0) (< ly 0) (>= uy 0))
         (make-interval (* ly ux) (* ux uy)))           ; x++ y+-
        ((and (>= lx 0) (>= ux 0) (< ly 0) (< uy 0))
         (make-interval (* ux ly) (* lx uy)))           ; x++ y--
        ((and (< lx 0)  (>= ux 0) (< ly 0) (>= uy 0))   ; x+- y+-
         (let ((p1 (* lx ly))
               (p2 (* lx uy))
               (p3 (* ux uy))
               (p4 (* ux ly)))
              (make-interval
               (min p2 p4)
               (max p1 p3))))
        ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
         (make-interval (* ux uy) (* lx ly)))           ; x-- y--
        ((and (< lx 0) (< ux 0) (< ly 0) (>= uy 0))
         (make-interval (* lx uy) (* lx ly)))           ; x-- y+-
        ((and (< lx 0)  (>= ux 0) (< ly 0) (< uy 0))
         (make-interval (* ly ux) (* ly lx))))))        ; x+- y--

(define (old-mul-interval x y)
 (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))
         
(define (div-interval x y)
 (let ((uby (upper-bound y)) (lby (lower-bound y)))
  (if (or (= uby 0) (= lby 0))
      "Divide by Zero"
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y)))))))

(let ((x (make-interval 1 3)) (y (make-interval 2 4)))
 (mul-interval x y))
(let ((x (make-interval 1 3)) (y (make-interval 2 4)))
 (old-mul-interval x y))

(let ((x (make-interval 1 3)) (y (make-interval -4 2)))
 (mul-interval x y))
(let ((x (make-interval 1 3)) (y (make-interval -4 2)))
 (old-mul-interval x y))

(let ((x (make-interval 1 3)) (y (make-interval -4 -2)))
 (mul-interval x y))
(let ((x (make-interval 1 3)) (y (make-interval -4 -2)))
 (old-mul-interval x y))

(let ((x (make-interval -3 1)) (y (make-interval 2 4)))
 (mul-interval x y))
(let ((x (make-interval -3 1)) (y (make-interval 2 4)))
 (old-mul-interval x y))

(let ((x (make-interval -3 1)) (y (make-interval -4 2)))
 (mul-interval x y))
(let ((x (make-interval -3 1)) (y (make-interval -4 2)))
 (old-mul-interval x y))

(let ((x (make-interval -3 1)) (y (make-interval -4 -2)))
 (mul-interval x y))
(let ((x (make-interval -3 1)) (y (make-interval -4 -2)))
 (old-mul-interval x y))

(let ((x (make-interval -3 -1)) (y (make-interval 2 4)))
 (mul-interval x y))
(let ((x (make-interval -3 -1)) (y (make-interval 2 4)))
 (old-mul-interval x y))

(let ((x (make-interval -3 -1)) (y (make-interval -4 2)))
 (mul-interval x y))
(let ((x (make-interval -3 -1)) (y (make-interval -4 2)))
 (old-mul-interval x y))

(let ((x (make-interval -3 -1)) (y (make-interval -4 -2)))
 (mul-interval x y))
(let ((x (make-interval -3 -1)) (y (make-interval -4 -2)))
 (old-mul-interval x y))

