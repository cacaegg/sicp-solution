(load "exec-5.22-machine.ss")

(define append-machine
  (make-machine
   '(ls1 ls2 result continue tmp)
   (list (list 'null? null?) (list 'cons cons)
	 (list 'car car) (list 'cdr cdr))
   '(start
     (assign continue (label done))
     (save continue)
     loop
     (test (op null?) (reg ls1))
     (branch (label base))
     (assign tmp (op car) (reg ls1))
     (save tmp)
     (assign continue (label after))
     (save continue)
     (assign ls1 (op cdr) (reg ls1))
     (goto (label loop))
     after
     (restore tmp)
     (assign result (op cons) (reg tmp) (reg result))
     (restore continue)
     (goto (reg continue))
     base
     (assign result (reg ls2))
     (restore continue)
     (goto (reg continue))
     done)))

(define append!-machine
  (make-machine
   '(ls1 ls2 continue result tmp)
   (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
   '(start
     (save ls1)
     (assign continue (label last-found))
     (save continue)
     (goto (label last-pair))
     last-found
     (perform (op set-cdr!) (reg ls1) (reg ls2))
     (restore result)
     (goto (label done))
     last-pair
     (assign tmp (op cdr) (reg ls1))
     (test (op null?) (reg tmp))
     (branch (label last-pair-done))
     (assign ls1 (op cdr) (reg ls1))
     (goto (label last-pair))
     last-pair-done
     (restore continue)
     (goto (reg continue))
     done)))

(append-machine 'trace-on)
;; ((append-machine 'trace-reg) 'continue)
;; ((append-machine 'trace-reg) 'tmp)
;; (set-breakpoint! append-machine 'right-done 4)
(set-register-contents! append-machine 'ls1 '(a b))
(set-register-contents! append-machine 'ls2 '(c d))
(start append-machine)
(get-register-contents append-machine 'result)

(append!-machine 'trace-on)
;; ((append-machine 'trace-reg) 'continue)
;; ((append-machine 'trace-reg) 'tmp)
;; (set-breakpoint! append-machine 'right-done 4)
(set-register-contents! append!-machine 'ls1 '(a b))
(set-register-contents! append!-machine 'ls2 '(c d))
(start append!-machine)
(get-register-contents append!-machine 'result)
