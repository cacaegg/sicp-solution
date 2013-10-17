(define (reverse ls)
 (let ((next (cdr ls)))
  (if (null? next)
      (list (car ls))
      (append (reverse next) 
              (list (car ls))))))

(reverse (list 3 4 5))
