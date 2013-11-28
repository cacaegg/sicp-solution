(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
 (set-car! (car x) 'wow)
 x)
z1
(set-to-wow! z1)

; |---|---|
; | | | | |
; |_|_|_|_|
;   |   |
;   v   v
; |-------|
; |  wow  |
; |_______|

z2
(set-to-wow! z2)

;  |---|---|     |---|---|    |---|---|
;  | | | ------> | | | -----> | | | X |
;  |_|_|___|     |_|_|___|    |_|_|___|
;    |             |            |
;    v             v            v
;  |-------|     |---|        |---|
;  |  wow  |     | a |        | b |
;  |_______|     |___|        |___|
