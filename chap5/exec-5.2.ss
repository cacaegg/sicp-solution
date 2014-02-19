(controller
 start
 (assign (reg p) (const 1))
 (assign (reg c) (const 1))
 test-cn
 (test (op >) (reg c) (reg n))
 (branch (label done))
 (assign t (op *) (reg p) (reg c))
 (assign p (reg t))
 (assign t (op +) (reg c) (const 1))
 (assign c (reg t))
 (goto (label test-cn))
 done)