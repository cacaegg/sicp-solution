(define (foreach func ls)
 (if (null? ls)
     #t
     (let ((next (cdr ls)))
          (func (car ls))
          (foreach next))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
