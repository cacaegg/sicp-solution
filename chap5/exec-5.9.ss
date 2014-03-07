(load "exec-5.9-machine.ss")

;;; Recursive exponentiation
(define double-label-machine
  (make-machine
   '(a b)
   (list (list '+ +) )
   '(start
     (assign a (op +) (const 1) (const 2))
     (assign b (op +) (label start) (const 3))
     there)))

(start double-label-machine)
(get-register-contents double-label-machine 'a)
(get-register-contents double-label-machine 'b)