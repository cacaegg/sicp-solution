; 1. For any process (exchange a b), the sum will still the same.
;    Hence, if all the exchange are run sequentially, the total 
;    can be preserved as original.
;  
; 2. Consider P1 run (exchange a b), and P2 run (exchange a c)
;    and a = 10, b = 20, c = 30.
;    
;    P1   read a = 10     >       read b = 20     >    set a = 20                set b = 10
;    P2               read a = 10                                   read c = 30              set a = 30  set c = 10
;    result > a = 30, b = 10, c = 10
;    The condition is violated
