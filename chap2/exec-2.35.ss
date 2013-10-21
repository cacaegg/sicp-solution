(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
 (accumulate +
             0 
             (map (lambda (node)
                          (if (pair? node) 
                              (count-leaves node)
                              1))
                  t)))

(count-leaves (list 1 2 (list 3 4) 5 6 (list 7 (list 8) 9)))
(count-leaves (list 1))
