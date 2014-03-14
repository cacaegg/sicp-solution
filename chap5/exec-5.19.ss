(load "exec-5.19-machine.ss")

(define gcd-machine
  (make-machine
   '(t b a)
   (list (list '= =) (list '< <) (list '- -))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (reg a))
     rem-loop
     (test (op <) (reg t) (reg b))
     (branch (label rem-done))
     (assign t (op -) (reg t) (reg b))
     (goto (label rem-loop))
     rem-done
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))


(gcd-machine 'trace-on)
(set-register-contents! gcd-machine 'a 16)
(set-register-contents! gcd-machine 'b 12)
(set-breakpoint! gcd-machine 'test-b 3)
(start gcd-machine)
(get-register-contents gcd-machine 'a)
(cancel-breakpoint! gcd-machine 'test-b 3)
(proceed-machine gcd-machine)
