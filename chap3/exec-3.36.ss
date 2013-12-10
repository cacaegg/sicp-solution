; (define a (make-connector))
; (define b (make-connector))
; (set-value! a 10 'user)
;
; |-------------------------------------------------------------------|
; | make-connector:..                                                 |  
; | ..                                                                |
; | a:                                                      b         |
; |   \                                                     |         |
; |____|____________________________________________________|_________|
;      |                        ^                           |        ^
;      |                        |                           |        |
;      |      |--------------------------------------|      v       |------------------------|
;     o o --->| value: 10         forget-my-value:.. |     o o ---> | value: #f         ...  |
;     |       | informant: user   set-my-value:..    |     |        | informant: #f          |
;     V       | constraints: '()  connect:..         |     |        | constraints: '()       |
; parameter:  |______________________________________|     |        |________________________|
;       request                                            |
; body:                                                   / 
;   (define me request ... <------------------------------
;
