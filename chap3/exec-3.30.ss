(define (half-adder a b s c)
 (let ((d (make-wire)) (e (make-wire)))
  (or-gate a b d)
  (and-gate a b c)
  (iverter c e)
  (and-gate d e s)
  'ok))
(define (full-adder a b c-in sum c-out)
 (let ((s (make-wire))
       (c1 (make-wire))
       (c2 (make-wire)))
  (half-adder b c-in s c1)
  (half-adder a s sum c2)
  (or-gate c1 c2 c-out)
  'ok))

(define (ripple-carry-adder an bn c-out sn)
 (define (add-iter an bn c-in sn)
  (if (null? (cdr an))
      (begin
       (full-adder (car an) (car bn) c-in (car s) c-out)
       'ok)
      (begin
       (make-wire carry-ab)
       (full-adder (car an) (car bn) c-in (car s) carry-ab)
       (add-iter (cdr an) (cdr bn) carry-ab) (cdr sn))))
 (add-iter an bn 0 sn c-out))

; 2 and-gate, 2 or-gate, and 1 inverter time delay per full-adder
; n-bit ripple-carry adder's delay will be time of 2*n and-gate, 2*n or-gate, n inverter 
