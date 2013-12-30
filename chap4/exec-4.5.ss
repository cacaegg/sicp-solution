;; Operation, type -> procedure
;; Dispatch table.
(define *op-table* (make-hashtable equal-hash equal?))
(define (put op type proc)
 (hashtable-set! *op-table* (list op type) proc))
(define (get op type)
 (hashtable-ref *op-table* (list op type) '()))

(define (type-tag exp) (car exp))

(put 'type-op 'and eval-and)
(put 'type-op 'or eval-or)
(put 'type-op 'if eval-if)
(put 'type-op 'begin eval-sequence)
(put 'type-op 'set! eval-assignment)
(put 'type-op 'define eval-definition)
(put 'type-op 'quote text-of-quotation)
(put 'type-op 'lambda make-procedure)
(put 'type-op 'cond 
     (lambda (exp env) (eval (cond->if exp) env)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'type-op (type-tag exp)) exp env)
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error 'eval "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-promitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error 'apply "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exp) (eval (first-exp exps)) env)
        (else (eval (first-step exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-and exp env)
  (cond ((null? (and-exps exp)) #t)
        (else
         (let ((exps (and-exps exp)))
           (let ((head (eval (and-head exps) env))
                 (rest (and-rest exps)))
             (if head
                 (if (null? rest)
                     head
                     (eval (make-and rest) env))
                 #f))))))

(define (eval-or exp env)
  (let ((exps (and-exps exp)))
    (cond ((null? exps) #f)
          (else
            (let ((head (eval (or-head exps) env))
                  (rest (or-rest exps)))
              (if head
                  head
                  (if (null? rest)
                      #f
                      (eval (make-or rest) env))))))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

;; Form (and exp1 ... expN)
(define (and-exps exp) (cdr exp))
(define (and-exps-head exp) (car exp))
(define (and-exps-rest exp) (cdr exp))
(define (make-and exp) (cons 'and exp))

;; Form (or exp1 ... expN)
(define (or-exps exp) (cdr exp))
(define (or-exps-head exp) (car exp))
(define (or-exps-rest exp) (cdr exp))
(define (make-or exp) (cons 'or exp))

;; Form (quote <text-of-quotation>)
(define (test-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; Form (set! <var> <value>)
(define (assignment? exp)
  (tagged-list exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Form (define <var> <value>)
;; or   (define (<var> <parameter1> ... <parameterN>) <body>)
(define (definition? exp)
  (tagged-list exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caddr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; formal parameters
                   (cddr exp))))    ; body

;; Lambda 
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Form (if <predicate?> <consequent> <alternative>)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) 
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; Begin (begine <sequence1> ... <sequenceN>)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; Procedure application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))

;; ======= Derived Experssions =======
;; Cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-test-recipient? exp) (eq? (cadr exp) '=>))
(define (cond-recipient exp) (caddr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'cond->if "ELSE clause isn't the last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (if (cond-test-recipient? first)
                         (list (cond-recipient first) (cond-predicate first))
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))
