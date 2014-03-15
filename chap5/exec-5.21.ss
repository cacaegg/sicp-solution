(load "exec-5.21-machine.ss")

(define count-leave-machine
  (make-machine
   '(tree val continue tmp)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?)
	 (list 'car car) (list 'cdr cdr) (list 'not not))
   '(start
     (assign continue (label count-done))
     (save continue)
     loop
     (test (op null?) (reg tree))
     (branch (label return-0))
     (assign tmp (op pair?) (reg tree))
     (test (op not) (reg tmp))
     (branch (label return-1))
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label left-done))
     (save continue)
     (goto (label loop))
     left-done
     (restore tree)
     (save val)
     (assign tree (op cdr) (reg tree))
     (assign continue (label right-done))
     (save continue)
     (goto (label loop))
     right-done
     (restore tmp)
     (assign val (op +) (reg val) (reg tmp))
     (restore continue)
     (goto (reg continue))
     return-0
     (assign val (const 0))
     (restore continue)
     (goto (reg continue))
     return-1
     (assign val (const 1))
     (restore continue)
     (goto (reg continue))
     count-done)))


(count-leave-machine 'trace-on)
;; ((count-leave-machine 'trace-reg) 'continue)
;; ((count-leave-machine 'trace-reg) 'tmp)
;; (set-breakpoint! count-leave-machine 'right-done 4)
(set-register-contents! count-leave-machine 'tree '(a (b c)))
(start count-leave-machine)
(get-register-contents count-leave-machine 'val)
