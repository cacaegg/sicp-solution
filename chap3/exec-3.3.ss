(define (make-account password balance)
 (define (balance-operator op amount)
  (set! balance (op balance amount))
  balance)
 (define (withdraw amount)
  (if (>= balance amount) 
      (balance-operator - amount)
      "Incufficient funds"))
 (define (deposit amount)
  (balance-operator + amount))
 (define (auth-failed msg) "Incorrect Password")
 (define (dispatch passwd msg)
  (cond ((not (eq? password passwd)) auth-failed)
        ((eq? msg 'withdraw) withdraw)
        ((eq? msg 'deposit) deposit)
        (else (error 'dispatch "Unknown request -- MAKE-ACCOUNT" m))))
 dispatch)

(define acc (make-account 'abcd 100))

((acc 'abcd 'withdraw) 10)

((acc 'aabcd 'withdraw) 10)
