; (import (chezscheme))
(define x 10)
(define f1 (lambda () (set! x (* x x))))
(define f2 (lambda () (set! x (+ x 1))))

(fork-thread f1)
(fork-thread f2)
x
