(load "exec-5.10-machine.ss")

;;; Recursive exponentiation
(define inc-machine
  (make-machine
   '(a)
   '()
   '(start
     (assign a (const 1))
     (inc (reg a))
     there)))

(start inc-machine)
(get-register-contents inc-machine 'a)