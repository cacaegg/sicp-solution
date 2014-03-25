;; preserving of oprator: env
;; for each operand: env, argl, proc

;; (f 'x 'y) no need env for operator, env, argl, proc for each operand
;; ((f) 'x 'y) no need env for operator, no need env, argl, proc for each operand
;; (f (g 'x) y) all needed
;; (f (g 'x) 'y) no need env for operator & operand