(define (make-withdraw init-amount)
 (let ((balance init-amount))
  (lambda (amount)
   (if (>= balance amount)
       (begin (set! balance (- balance amount))
              balance)
       "Insufficient funds"))))

; Since let is syntax sugar of lambda, it can be rewritten as:

(define (make-withdraw init-amount)
 ((lambda 
   (balance)
   (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
  init-amount))

(define W1 (make-withdraw 100))
; Hence, this introduce another new environment frame when creating withdraw
;
;|-------------------------|
;|make-withdraw: ...------------------------------> O parameter: init-amount
;|W1:                      |                        O      body: ((lambda (balance)
;|   \                     |    |---------------|   |                     (lambda (amount)
;|____|____________________|<---|init-amount:100|<---                             (if (>= balance amount)
;     |                         |lambda:...     |
;     |                         |__________\____|                                     (begin (set! balance (- balance amount))
;     |                             ^       \                                                balance)
;     |                             |        \                                        "Insufficient funds")))
;     |                             |         \                   init-amount)
;     |                       |------------|   \
;     |                       | balance:100|    \
;     |   ----------------------lambda:    |    |
;     |  /                    |____________|     \  
;     v v                       ^      ^          \
;    O O------------------------|      |           ------> O parameter: balance
;  parameter: amount                   |                   O      body: (lambda (amount)
;  body:                               |__________________/                (if (>= balance amount)
;    (if (>= balance amount)                                                   (begin (set! balance (- balance amount))
;        (begin (set! balance (- balance amount))                                     balance)
;               balance)                                                       "Insufficient funds"))
;        "Insufficient funds")

