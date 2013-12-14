; No Ben is right.
; The major difference between transfer and exchange is that
; transfer only need to withdraw from one account and deposit to another,
; which both are serialized.
; However, the exchange procedure need to read balance for both account,
; this can be bad if some other exchange process is also exchange one of the account.
