(define us-coins (list 50 25 10 5 1))
(define us-coins-p (list 25 10 5 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-ls)
 (null? coin-ls))
(define (first-denomination coin-ls)
 (car coin-ls))
(define (except-first-denomination coin-ls)
 (cdr coin-ls))

(define (cc amount coin-values)
 (cond ((= amount 0) 1)
       ((or (< amount 0) (no-more? coin-values)) 0)
       (else
        (+ (cc amount
               (except-first-denomination coin-values))
           (cc (- amount
                  (first-denomination coin-values))
               coin-values)))))

(cc 100 us-coins)
(cc 100 us-coins-p)
; The order of coins in list doesn't change the answer produce by cc
; Since in the "else", it will sum all types of combination recursively.
; e.g, if coins is (10 1) or (1 10), then else will be like
; (10 1) -> {(1), (10,1)}  
;             |      |
;             |      > All type of 10 & 1 (Include 10 only)
;             v
;             Coin 1 only
;
; (1 10) -> {(10), (1, 10)}
;             |      |
;             |      > All type of 1 & 10 (Include 1 only)
;             v
;             Coin 10 only
