(define (split topainter tosmaller)
 (lambda (painter n)
  (if (= n 0)
      painter
      (let ((smaller (split painter (- n 1))))
           (topainter painter (tosmaller smaller smaller))))))
