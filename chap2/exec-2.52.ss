(define (corner-split painter n)
 (if (= n 0)
     painter
     (beside 
      (below painter
             (up-split painter (- n 1)))
      (below (right-split painter (- n 1))
             (corner-split painter (- n 1))))))

(define (square-limit painter n)
 (let ((quarter (coner-split painter n)))
  (square-of-four 
   (flip-horiz quarter)
   quarter
   (rot180 quarter)
   (flip-vert quarter))))
