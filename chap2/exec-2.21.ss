(define (square-list-1 items)
 (if (null? items)
     '()
     (cons (* (car items) (car items)) 
           (square-list-1 (cdr items)))))

(define (square-list-2 items)
 (map (lambda (x) (* x x))
      items))

(square-list-1 (list 1 2 3 4 5))
(square-list-2 (list 1 2 3 4 5))
