(define (or-gate a1 a2 output)
 (let ((a3 (make-wire)) 
       (a4 (make-wire))
       (a5 (make-wire)))
  (inverter a1 a3)
  (inverter a2 a4)
  (and-gate a3 a4 a5)
  (inverter a5 output)
  'ok))

; or-gate will delay by 3 inverter and 1 and-gate
