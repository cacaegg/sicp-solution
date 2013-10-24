(define (up-split paiter n)
 (if (= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
