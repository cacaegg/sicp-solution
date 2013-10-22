(define (reverse ls)
 (let ((next (cdr ls)))
  (if (null? next)
      (list (car ls))
      (append (reverse next) 
              (list (car ls))))))

(reverse (list 3 4 5))

(define (reverse ls)
 (fold-right (lambda (new result) 
                     (append result (list new))) 
             '() 
             ls))

(reverse (list 3 4 5))


(define (reverse ls)
 (fold-left (lambda (result new) 
                    (append (list new) result))
            '() 
            ls))
(reverse (list 3 4 5))


