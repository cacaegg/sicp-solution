;;; n = 3
;;; Stack Top () Bottom
(assign continue (label fib-done))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(save continue)
;;; Stack ((label fib-done))
(assign continue (label afterfib-n-1)) ;; continue = afterfib-n-1
(save n)
;;; Stack (3 (label fib-done))
(assign n (op -) (reg n) (const 1)) ;; n = 2
(goto (label fib-loop))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(save continue)
;;; Stack ((label afterfib-n-1) 3 (label fib-done))
(assign continue (label afterfib-n-1)) ;; continue = afterfib-n-1
(save n)
;;; Stack (2 (label afterfib-n-1) 3 (label fib-done))
(assign n (op -) (reg n) (const 1)) ;; n = 1
(goto (fib-loop))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n))  ;; val = 1
(goto (reg continue)) 
(restore n) ;; n = 2
;;; Stack ((label afterfib-n-1) 3 (label fib-done))
(restore continue) ;; continue = afterfib-n-1
;;; Stack (3 (label fib-done))
(assign n (op -) (reg n) (const 2)) ;; n = 0
(save continue) 
;;; Stack ((label afterfib-n-1) 3 (label fib-done))
(assign continue (label afterfib-n-2)) ;; continue = afterfib-n-2
(save val) 
;;; Stack  (1 (label afterfib-n-1) 3 (label fib-done))
(goto (label-fib-loop))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n)) ;; val = 0
(goto (reg continue))
(assign n (reg val)) ;; n = 0
(restore val) ;; val = 1
;; Stack ((label afterfib-n-1) 3 (label fib-done))
(restore continue) ;; continue = afterfib-n-1
;; Stack (3 (label fib-done))
(assign val
	(op +) (reg val) (reg n)) ;; val = 1
(goto (reg continue))
(restore n) ;; n = 3
;; Stack ((label fib-done))
(restore continue) ;; continue = fib-done
;; Stack ()
(assign n (op -) (reg n) (const 2)) ;; n = 1
(save continue)
;; Stack ((label fib-done))
(assign continue (label afterfib-n-2)) ;; continue = afterfib-n-2
(save val)
;; Stack (1 (label fib-done))
(goto (label fib-loop))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n)) ;; val = 1
(goto (reg continue))
(assign n (reg val)) ;; n = 1
(restore val) ;; val = 1
(restore continue) ;; continue = fib-done
(assign val
	(op +) (reg val) (reg n)) ;; val = 2
(goto (reg continue))
;; fib-done, val = 2

