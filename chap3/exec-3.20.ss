(define (cons x y)
 (define (set-x! v) (set! x v))
 (define (set-y! v) (set! y v))
 (define (dispatch m)
  (cond ((eq? m 'car) x)
        ((eq? m 'cdr) y)
        ((eq? m 'set-car!) set-x!)
        ((eq? m 'set-cdr!) set-y!)
        (else
         (error 'cons "Undefiend operation -- CONS" m))))
 dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
 ((z 'set-car!) new-value)
 z)
(define (set-cdr! z new-value)
 ((z 'set-cdr!) new-value)
 z)


(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(car x)

;  |-----------------------------------------------------|
;  | cons:---|          set-car!:...    x:--|    y:...   |
;  | car:..  |          set-cdr!:...        |            |
;  | cdr:..  |                              |            |
;  |_________|_______________^______________|____________|
;            |               |              |
;            v           |----------|       v
;           O O          |set-x!:   |       O O -------------------|  |------|
;       paramter: x y    |set-y!:   |     parameter: m             |  |m:'car|
;                        |dispatch: |     body:                    |  |______|
;       body:            |x:1  y:2  |       (cond ((eq? m 'car) x) |      |    
;                        |__________|<--|         ....             |      |
;           (define set-x!...     ^     |__________________________|      |    
;           (define set-y!...     |_______________________________________|
;           (define dispatch m
;           dispatch


