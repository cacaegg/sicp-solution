;;; L-Eval input:
(define count 0)

;;; L-Eval value:
ok

;;; L-Eval input:
(define (id x)
  (set! count (+ count 1))
  x))

;;; L-Eval value:
ok

;;; L-Eval input:
(define w (id (id 10)))

;;; L-Eval value:
ok

; At this time, define w will evaluate outer (id (id 10)), the inner is treat as delayed argument.

;;; L-Eval input:
count

;;; L-Eval value:
1


;;; L-Eval input:
w

;;; L-Eval value:
10


; Inner delayed (id 10) is evaluated now

;;; L-Eval input:
count

;;; L-Eval value:
2
