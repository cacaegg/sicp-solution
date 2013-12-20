(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-stream (stream-cdr fibs)
                                        fibs))))
; (0 1 1 2 ...
;
; need of plus for fibs: 
; plus(n) = plus(n-1) + plus(n-2) +1
; Where plus(0) = 0, plus(1) = 0
;
; If we use memo-proc, 
; plus(n) = plus(n-1) + 1
; where plus(0) = 0, plus(1) = 0
