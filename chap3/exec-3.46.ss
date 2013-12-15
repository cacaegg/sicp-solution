;
; P1 test-and-set (car cell) > #f
; P2 test-and-set (car cell) > #f
; P1 (begin (set-car! cell #t) #f)
; P2 (begin (set-car! cell #t) #f)
;
; Then P1 and P2 will get the mutex at the same time
