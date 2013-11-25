(define (make-account password balance)
 (define password-list (list password))
 (define (balance-operator op amount)
  (set! balance (op balance amount))
  balance)
 (define (withdraw amount)
  (if (>= balance amount) 
      (balance-operator - amount)
      "Incufficient funds"))
 (define (deposit amount)
  (balance-operator + amount))

 (define failed-times 0)
 (define (auth-failed msg) 
  (if (< failed-times 7)
      (format "Incorrect Password (~s times)" failed-times)
      (call-the-cops)))
 (define (call-the-cops) "Call the COPS!!!")
 (define (auth-ok? passwd)
  (define (auth-iter remain-password)
   (cond ((null? remain-password)
          (begin
           (set! failed-times (1+ failed-times)) #f))
         ((eq? (car remain-password) passwd)
          (begin
           (set! failed-times 0) #t))
         (else
          (auth-iter (cdr remain-password)))))
  ;(display (list "auth-ok?" password-list passwd))(newline)
  (auth-iter password-list))
 (define (add-password new-pass)
  (set! password-list (append password-list (list new-pass))))

 (define (dispatch passwd msg)
  (cond ((eq? msg 'pass-ok?) (auth-ok? passwd))
        ((not (auth-ok? passwd)) auth-failed)
        ((eq? msg 'withdraw) withdraw)
        ((eq? msg 'deposit) deposit)
        ((eq? msg 'add-passwd) add-password)
        (else (error 'dispatch "Unknown request -- MAKE-ACCOUNT" m))))
 dispatch)

(define (make-joint orig-acc orig-pass new-pass)
 (if (acc orig-pass 'pass-ok?)
     (begin
      ((acc orig-pass 'add-passwd) new-pass)
      orig-acc)
     "Failed to make a joint account"))

(define acc (make-account 'abcd 100))

((acc 'abcd 'withdraw) 10)

((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)


(define bcc (make-joint acc 'abcd 'keroro))

((acc 'abcd 'deposit) 200)
((bcc 'keroro 'deposit) 50)

((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((bcc 'aabcd 'withdraw) 10)
((acc 'aabcd 'withdraw) 10)
((bcc 'aabcd 'withdraw) 10)
((bcc 'aabcd 'withdraw) 10)
