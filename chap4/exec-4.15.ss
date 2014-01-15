;; (define (run-forever) (run-forever))
;;
;; (define (try p)
;;   (if (halt? p p)
;;       (run-forever)
;;       'halted))
;;
;; If we run (try try) then it can expand to
;;
;; (if (halt? try try)
;;     (run-forever)
;;     'halted)
;;
;; Since halt? need to know if (try try) is halted or not,
;; it need to find out again (Recursive forever on halt? problem)
;;
;; And halt? procedure can't never be solved,
;; nor (run-forever) or 'halted will be reached.
;;
;; For (halt? try try) is true, it means this can be returned.
;; But the procedure will (run-forever), which is contracted.
;;
;; On the otherside, if (halt? try try) return false, the 
;; (try try) result is 'halted, which is also contracted.

