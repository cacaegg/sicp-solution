(define (rand-generator)
 (define x 0)
 (define (reset new-val)
  (set! x new-val))
 (define (rand-update new-x)
  (mod (+ (* 312311 new-x) 7327392) 113837))
 (define (generate)
  (set! x (rand-update x))
  x)
 (lambda (op)
  (cond ((eq? op 'reset) reset)
        ((eq? op 'generate) (generate))
        (else
         (error 'rand-generator "ERROR: no such operation -- RAND-GENERATOR" op)))))

(define r (rand-generator))

(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)

((r 'reset) 0)

(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)
(r 'generate)


