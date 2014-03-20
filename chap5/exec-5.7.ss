(load "ch5-regsim.scm")

;;; Recursive exponentiation
(define recfib-machine
  (make-machine
   '(n continue val b tmp)
   (list (list '= =) (list '- -) (list '* *))
   '(expt-start
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign continue (label after-expt))
     (assign n (op -) (reg n) (const 1))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg val) (reg b))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done
    )))

(set-register-contents! recfib-machine 'n 3)
(set-register-contents! recfib-machine 'b 2)
(start recfib-machine)
(get-register-contents recfib-machine 'val)

;;; Iterative exponentiation
(define iterexpt-machine
  (make-machine
   '(p c n b)
   (list (list '= =) (list '* *) (list '- -))
   '(expt-start
     (assign p (const 1))
     (assign c (reg n))
     expt-loop
     (test (op =) (reg c) (const 0))
     (branch (label expt-done))
     (assign p (op *) (reg p) (reg b))
     (assign c (op -) (reg c) (const 1))
     (goto (label expt-loop))
     expt-done)))

(set-register-contents! iterexpt-machine 'n 3)
(set-register-contents! iterexpt-machine 'b 2)
(start iterexpt-machine)
(get-register-contents iterexpt-machine 'p)