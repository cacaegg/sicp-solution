(define (gcd a b)
 (if (= b 0)
     a
     (gcd b (remainder a b))))

; Normal-order evaluation
; (gcd 206 40)
; (gcd 40 (remainder 206 40)) ; 1
; (gcd (remainder 206 40) 
;      (remainder 40 (remainder 206 40))) ; 2
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40)
;                 (remainder 40 (remainder 206 40)))) ; 4
; (gcd (remainder (remainder 206 40)
;                 (remainder 40 (remainder 206 40))) ; 4
;      (remainder (remainder 40 (remainder 206 40))  
;                 (remainder (remainder 206 40)  
;                            (remainder 40 (remainder 206 40))))) ; 7
; 18 times
;
; Applicative-order evaluation
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 6 (remainder 40 6))
; (gcd 4 (remainder 6 4))
; (gcd 2 (remainder 4 2))
; 4 times
(gcd 206 40)
