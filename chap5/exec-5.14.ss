;; totoal push = n * 2 - 2
;; max stack depth = n * 2 -2
(load "exec-5.14-machine.ss")

(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done)))

(set-register-contents! fact-machine 'n 1)
(start fact-machine)
(get-register-contents fact-machine 'val)
(print-stat fact-machine)

(reset-machine fact-machine)

(set-register-contents! fact-machine 'n 2)
(start fact-machine)
(get-register-contents fact-machine 'val)
(print-stat fact-machine)


(reset-machine fact-machine)

(set-register-contents! fact-machine 'n 4)
(start fact-machine)
(get-register-contents fact-machine 'val)
(print-stat fact-machine)


(reset-machine fact-machine)

(set-register-contents! fact-machine 'n 8)
(start fact-machine)
(get-register-contents fact-machine 'val)
(print-stat fact-machine)


(reset-machine fact-machine)

(set-register-contents! fact-machine 'n 16)
(start fact-machine)
(get-register-contents fact-machine 'val)
(print-stat fact-machine)