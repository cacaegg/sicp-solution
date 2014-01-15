(define (fib n) 
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (fib (- n 1)) (fib (- n 2))))))

(fib 28)


; (fib, n) => original version, analyze version
; (fib, 1) => 0m0.206s, 0m0.208s
; (fib, 2) => 0m0.201s, 0m0.200s
; (fib, 4) => 0m0.200s, 0m0.200s
; (fib, 8) => 0m0.203s, 0m0.200s
; (fib, 16) => 0m0.313s, 0m0.247s
; (fib, 24) => 0m5.089s, 0m2.084s
; (fib, 28) => 0m33.536s, 0m13.685s
