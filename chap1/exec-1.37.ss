(define (cont-frac-rec n d k)
  (if (< k 1)
      0
      (/ (n k) 
         (+ (d k)
            (cont-frac-rec n d (1- k))))))

(cont-frac-rec (lambda (x) 1.0)
               (lambda (x) 1.0)
               6)

(define (cont-frac-iter n d k)
 (define (iter cur result)
  (if (< cur 1)
      result
      (iter (1- cur)
            (/ (n cur)
               (+ (d cur)
                  result)))))
 (iter k 0))

(cont-frac-iter (lambda (x) 1.0)
                (lambda (x) 1.0)
                6)
