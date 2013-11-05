(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; Operation, type -> procedure
;; ;; Dispatch table.
;; ;; 
(define *op-table* (make-hashtable equal-hash equal?))
(define (put op type proc)
 (hashtable-set! *op-table* (list op type) proc))
(define (get op type)
 (hashtable-ref *op-table* (list op type) '()))

(define (deriv exp var)
 (cond ((number? exp) 0)
       ((variable? exp) (if (same-variable? exp var) 1 0))
       (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a. This procedure check if exp is not 0 & variable, then get operator 
;    procedure in the table by deriv & tag in exp. Then pass the operand
;    and var the operator procedure.
;
;    We can add number? & same-variable? into data-directed dispath. But
;    since such comparision happens in all operation of deriv, it's better
;    to make it at the deriv procedure itself.
; 
; b. c.
(define (install-deriv)
 ;; internal procedure
 (define (=number? exp num)
   (and (number? exp) (= exp num)))

 (define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
         ((=number? a2 0) a1)
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list '+ a1 a2))))
 (define (make-product m1 m2)
   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((and (number? m1) (number? m2)) (* m1 m2))
         (else (list '* m1 m2))))
 (define (make-exponentiation b e)
   (cond ((=number? e 0) 1)
         ((=number? e 1) b)
         ((and (number? b) (number? e)) (expt b e))
         (else (list '** b e))))

 (define (addend s) (car s))
 (define (augend s) (cadr s))
 (define (multiplier p) (car p))
 (define (multiplicand p) (cadr p))
 (define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))
 (define (base e) (car e))
 (define (exponent e) (cadr e))

 (define (sum-operator exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))
 (define (product-operator exp var)
  (make-sum 
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))
 (define (expo-operator exp var)
  (make-product
   (make-product
    (exponent exp)
    (make-exponentation
     (base exp)
     (- (exponent exp) 1)))
   (deriv (base exp) var)))
   

 ;; interfaces
 (put 'deriv '+ sum-operator)
 (put 'deriv '* product-operator)
 (put 'deriv '** expo-experator)
 'done)

(install-deriv)
(deriv '(* (* x y) (+ x 3)) 'x)

; Only need to change the order of put in install package
