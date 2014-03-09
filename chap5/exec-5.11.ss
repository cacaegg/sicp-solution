(load "exec-5.11-machine-a.ss")

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

(set-register-contents! fib-machine 'n 4)
(start fib-machine)
(get-register-contents fib-machine 'val)

(load "exec-5.11-machine-b.ss")

(define sync-push-pop-machine
  (make-machine
   '(x y)
   '()
   '(start
     (assign x (const 3))
     (assign y (const 4))
     (save x)
     (restore y))))

(start sync-push-pop-machine)

(load "exec-5.11-machine-c.ss")

(define multi-stack-machine
  (make-machine
   '(x y)
   '()
   '(start
     (assign x (const 3))
     (assign y (const 4))
     (save x)
     (save y)
     (save y)
     (restore x)
     (restore y))))

(start multi-stack-machine)
(get-register-contents multi-stack-machine 'x)
(get-register-contents multi-stack-machine 'y)