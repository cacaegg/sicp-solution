(define (last-pair ls)
 (let ((next (cdr ls)))
      (if (null? next)
          (car ls)
          (last-pair next))))

(last-pair (list 93 382 812 33))
