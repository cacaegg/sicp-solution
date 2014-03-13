(load "exec-5.18-machine.ss")

(define fib-machine
  (make-machine
   '(continue n val)
   (list (list '+ +) (list '< <) (list '- -))
   '(start
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     afterfib-n-1
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val
	     (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

(fib-machine 'trace-on)
(set-register-contents! fib-machine 'n 3)
(start fib-machine)
(get-register-contents fib-machine 'val)
(fib-machine 'print-stat)


(fib-machine 'reset)
(fib-machine 'trace-off)
(set-register-contents! fib-machine 'n 3)
(start fib-machine)
(get-register-contents fib-machine 'val)
(fib-machine 'print-stat)