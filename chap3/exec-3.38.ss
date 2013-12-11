; Peter: (set! balance (+ balance 10))
; Paul : (set! balance (- balance 20))
; Mary : (set! balance (- balance (/ balance 2)))
; a. 4 possible results
;    /2, +10, -20 -> 40
;    -20, /2, +10 -> 50
;    +10, /2, -20 -> 35
;    +10, -20, /2 -> 45
;
; b. -------------------Time-Flow------------------------>
;    
;    Pter       +10
;             /    \
;    Bank  100      110 120   60 
;             \        /  |   |
;    Paul       -20 ---   |   |
;                          \  /
;    Mary                   /2
