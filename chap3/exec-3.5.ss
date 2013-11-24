(define (square x) (* x x))

(define (random-in-range low high)
 (let ((range (- high low)))
  (+ (random range) low)))

(define (estimate-integral P? x1 x2 y1 y2 trail-num)
 (let ((in-cir? (P? x1 x2 y1 y2)))
  (let ((integral-test 
         (lambda ()
          (in-cir? (random-in-range x1 x2) (random-in-range y1 y2)))))
   (* (* (abs (- x2 x1)) (abs (- y2 y1)))
      (monte-carlo trail-num integral-test)))))

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

(define (monte-carlo trails experiment)
 (define (iter trails-remaining trail-passed)
  (cond ((= trails-remaining 0)
         (/ trail-passed trails))
        ((experiment)
         (iter (1- trails-remaining) (1+ trail-passed)))
        (else
         (iter (1- trails-remaining) trail-passed))))
 (iter trails 0))

(estimate-integral in-circle? -1.0 1.0 -1.0 1.0 1000000)
