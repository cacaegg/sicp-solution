(load "exec-5.8-machine.ss")

;;; Recursive exponentiation
(define double-label-machine
  (make-machine
   '(a)
   '()
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))

(start double-label-machine)
(get-register-contents double-label-machine 'a)

;; a will be 3 if label duplicate is allowed
