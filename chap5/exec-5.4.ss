;;; Recursive exponentiation
(controller
 (assign continue (label expt-done))
 expt-start
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (assign continue (label after-expt))
 (assign n (op -) (reg n) 1)
 (goto (label expt-start))
 after-expt
 (restore continue)
 (assign val (op *) (reg val) (reg b))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 expt-done
 )

;;; Iterative exponentiation
(controller
 expt-start
 (assign p (const 1))
 (assign c (reg n))
 expt-loop
 (test (op =) (reg c) (const 0))
 (branch (label expt-done))
 (assign p (op *) (reg p) (reg b))
 (assign c (op -) (reg c) (const 1))
 (goto (label expt-loop))
 expt-done)