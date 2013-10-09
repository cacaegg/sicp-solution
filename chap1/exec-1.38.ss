(define (cont-frac-iter n d k)
 (define (iter cur result)
  (if (< cur 1)
      result
      (iter (1- cur)
            (/ (n cur)
               (+ (d cur)
                  result)))))
 (iter k 0))

; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8
(define (fd i)
 (cond ((= (mod i 3) 2) 
        (* 2 (/ (1+ i) 3)))
       (else 1)))

; e - 2 = 0.71828182845
(cont-frac-iter (lambda (x) 1.0)
                fd
                120)
