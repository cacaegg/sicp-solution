(define (tan-cf x k)
 (define (iter cur result)
  (if (< cur 1)
      result
      (iter (1- cur)
            (/ (cond ((= cur 1) x)
                     (else (* x x)))
               (- (1+ (* 2 (1- cur)))
                  result)))))
 (iter k 0.0))

(tan-cf 90 30000)
