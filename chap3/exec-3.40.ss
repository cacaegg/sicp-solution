; (define x 10)
; (parallel-execute (lambda () (set! x (* x x)))     Ra1 Ra2 Sa1
;                   (lambda () (set! x (* x x x))))  Rb1 Rb2 Rb3 Sb1
;
; (Rb1, Rb2, Rb3) can be (10, 10, 10) (100, 10, 10) (100, 100, 10) (100, 100, 100)
; 
; (Ra1, Ra2) can be (10, 10) (1000, 10) (1000, 1000)
; 
; (10, 10, 10) (10, 10) Sb1 Sa1     > 100
;                       Sa1 Sb1     > 1000
; (10, 10, 10) (1000, 10) Sa1       > 10000
; (10, 10, 10) Sb1 (1000, 1000) Sa1 > 1000000
;
; (10, 10) (100, 10, 10) Sb1        > 10000
; (10, 10) (100, 100, 10) Sb1       > 100000
; (10, 10) Sa1 (100, 100, 100) Sb1  > 100000000
; 6 possible values
;
; (define s (make-serializer))
; (parrallel-execute (s (lambda () (set! x (* x x))))
;                    (s (lambda () (set! x (* x x x)))))
; (10, 10) Sa1 (100, 100, 100) Sb1  > 100000000
; (10, 10, 10) Sb1 (1000, 1000) Sa1 > 1000000
; 2 possible values
;
; 
