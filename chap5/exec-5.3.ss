(controller
 sqrt-start
 (assign x (op read))
 (assign g (const 1))
 sqrt-loop
 good-enough
 (assign t1 (op square) (reg g))
 (assign t2 (op -) (reg t1) (reg x))
 (assign t1 (op abs) (reg t2))
 (test (op <) (reg t1) (const 0.001))
 good-enough-done
 (branch (label sqrt-done))
 improve
 (assign t2 (op /) (reg g) (reg x))
 (assign t1 (op average) (reg g) (reg t2))
 improve-done
 (assign g (reg t1))
 (goto (label sqrt-loop))
 sqrt-done
 (perform (op print) (reg g))
 (goto (label sqrt-start)))