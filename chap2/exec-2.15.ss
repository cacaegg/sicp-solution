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

(define (div-interval x y)
 (let ((uby (upper-bound y)) (lby (lower-bound y)))
  (if (or (= uby 0) (= lby 0))
      "Divide by Zero"
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y)))))))

; p is percent. 1 for 100%, 0.1 for 10%
(define (make-center-percent c p)
 (make-interval (- c (* c p)) (+ c (* c p))))

(define (center i)
 (/ (+ (upper-bound i) (lower-bound i)) 2.0))

(define (percent i)
 (/ (- (center i) (lower-bound i)) (center i)))

(define (par1 r1 r2)
 (div-interval (mul-interval r1 r2)
               (add-interval r1 r2)))

(define (par2 r1 r2)
 (let ((one (make-interval  1 1)))
  (div-interval one
                (add-interval (div-interval one r1)
                              (div-interval one r2)))))

(let ((i1 (make-center-percent 20000 0.00001))
      (i2 (make-center-percent 40000 0.00002)))
 (par1 i1 i2))
(let ((i1 (make-center-percent 20000 0.00001))
      (i2 (make-center-percent 40000 0.00002)))
 (par2 i1 i2))

; Given R1 = A, R2 = 1/A
; par1 => (A * 1/A) / (A + 1/A)
; par2 => 1 / (1/A + A)
; par1 has the unprecised part (A * 1/A), so par2 is better. 
