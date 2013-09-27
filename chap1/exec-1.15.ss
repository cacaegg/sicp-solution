(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
 (if (not (> (abs angle) 0.1))
     angle
     (p (sine (/ angle 3.0)))))

; 12.15 -> 4.05 -> 1.34999 -> 0.44999 -> 0.15 -> 0.04 
; p procedure will be applied for 5 times
(sine 12.15)

; Order growth of Space -> O(log(n))
; Due to the depth of recursive tree is grow for each 3*n
;
; Order growth of Time -> O(log(n))
; Since each p procedure is constant, it will grow with p procedure
