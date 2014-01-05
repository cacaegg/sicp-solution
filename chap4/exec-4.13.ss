(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  ; (display (list 'apply))(newline)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error 'apply "Unknown procedure type -- APPLY" procedure))))

(define (eval exp env)
  ; (display (list 'eval exp (unbound? exp)))(newline)
  (cond ((self-evaluating? exp) exp)
        ((unbound? exp) (make-unbound! exp env))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((for? exp)
         (eval (for->recur exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error 'eval "Unknown expression type -- EVAL" exp))))

; Argument evaluation, left to right
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((val (eval (first-operand exps) env)))
        (cons val
              (list-of-values-lr (rest-operands exps) env)))))

; Argument evaluation, right to left
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((r-val (list-of-values-rl (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) 
              r-val))))

(define list-of-values list-of-values-rl)

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  ; (display (list 'eval-sequence exps))(newline)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  ; (display (list 'eval-definition exp (eval (definition-value exp) env)))(newline)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-and exp env)
  (cond ((null? (and-exps exp)) #t)
        (else
         (let ((exps (and-exps exp)))
           (let ((head (eval (and-exps-head exps) env))
                 (rest (and-exps-rest exps)))
             (if head
                 (if (null? rest)
                     head
                     (eval (make-and rest) env))
                 #f))))))

(define (eval-or exp env)
  (let ((exps (or-exps exp)))
    (cond ((null? exps) #f)
          (else
            (let ((head (eval (or-exps-head exps) env))
                  (rest (or-exps-rest exps)))
              (if head
                  head
                  (if (null? rest)
                      #f
                      (eval (make-or rest) env))))))))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((eq? exp #t) #t)
        ((eq? exp #f) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

;; Form (quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; Form (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Form (define <var> <value>)
;; or   (define (<var> <parameter1> ... <parameterN>) <body>)
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  ; (display (list 'definition-value exp (cadr exp)))(newline)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; formal parameters
                   (cddr exp))))    ; body

;; Lambda 
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (caddr exp))
(define (make-lambda parameters body)
  (list 'lambda parameters body))

;; Form (and exp1 ... expN)
(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (and-exps-head exp) (car exp))
(define (and-exps-rest exp) (cdr exp))
(define (make-and exp) (cons 'and exp))

;; Form (or exp1 ... expN)
(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (or-exps-head exp) (car exp))
(define (or-exps-rest exp) (cdr exp))
(define (make-or exp) (cons 'or exp))

;; Form (if <predicate?> <consequent> <alternative>)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) 
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (if (null? alternative)
      (list 'if predicate consequent)
      (list 'if predicate consequent alternative)))

;; Begin (begine <sequence1> ... <sequenceN>)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  ; (display (list 'sequence->exp seq))(newline)
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
(define (rest-operands ops) (cdr ops))

;; Testing predicates
(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

;; Handle primitive procedures
;; (apply-primitive-procedure <proc> <args>)
;; (primitive-procedure? <proc>)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list 'list list)
        (list 'display display)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *) 
        (list '/ /)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
;; Construct compound procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Operations on environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values) 
  (cons 'frame (map cons variables values)))
(define (frame-pairs frame) (cdr frame))
(define (frame-variables frame) (map car (frame-pairs frame)))
(define (frame-values frame) (map cdr (frame-pairs frame)))
(define (add-binding-to-frame! var val frame)
  ; (display (list 'add-binding var val))(newline)
  (let ((new-pair (cons var val)))
    (set-cdr! frame (cons new-pair (cdr frame)))))
;; (extend-environment <variables> <values> <base-env>)
(define (extend-environment vars vals base-env)
  ; (display (list 'ext-env vars vals))(newline)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error 'extend-environment "Too many argument supplied" vars vals)
          (error 'extend-environment "Too few arguments supplied" vars vals))))
(define (env-loop var env proc-null proc-find)
  (define (scan vars vals)
    (cond ((null? vars)
           (proc-null env))
          ((eq? var (car vars))
           (proc-find vals vals))
          (else
           (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error 'env-loop "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
;; (lookup-variable-value <var> <env>)
(define (lookup-variable-value var env)
  (define (proc-null env)
    (env-loop var (enclosing-environment env) proc-null proc-find))
  (define (proc-find vals val) (car vals))
  (env-loop var env proc-null proc-find))
;; (define-variable! <var> <value> <env>)
(define (define-variable! var val env)
  (define (proc-null env)
    (add-binding-to-frame! var val (first-frame env)))
  (define (proc-find vals val) (set-car! vals val))
  (env-loop var env proc-null proc-find))
;; (set-variable-value <var> <value> <env>)
(define (set-variable-value! var val env)
  (define (proc-null env)
    (env-loop var env (enclosing-environment env) proc-null proc-find))
  (define (proc-find vals val) (set-car! vals val))
  (env-loop var env proc-null proc-find))

;; (make-unbound! var env)
(define (unbound? exp) (tagged-list? exp 'make-unbound!))
(define (make-unbound! exp env)
  ; (display (list 'make-unbound! exp))(newline)
  (let ((var (cadr exp)))
    (define (loop pairs)
      (cond ((null? pairs) '())
            ((eq? (caar pairs) var)
             (cdr pairs))
            (else
             (cons (car pairs) (loop (cdr pairs))))))
    (let* ((frame (first-frame env))
           (new-pairs (loop (frame-pairs frame))))
      (if (null? new-pairs)
          (error 'make-unbound "Variable is not bounded" var)
          (set-cdr! frame new-pairs))
      'done)))
;; Remove the bounded variable only in top frame of environment
;; To avoid variable accendently remove by inner procedure.
;; e.g. If Peter calls Bill's procedure, then Bill should remove
;; only its own bounded variables (inner) rather than Peter's 
;; variable (outer).



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

;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-name? exp) (symbol? (cadr exp)))
(define (let-name exp) (cadr exp))
(define (let-bind-exps exp) 
  (if (let-name? exp) (caddr exp) (cadr exp)))
(define (let-bind-var exp) (car exp))
(define (let-bind-exp exp) (cadr exp))
(define (let-body exp) 
  (if (let-name? exp)
      (cadddr exp)
      (cddr exp)))
(define (let->combination exp)
  ; (display (list 'let->combination exp))(newline)
  (if (let-name? exp)
      (sequence->exp
        (list 
          (list 'define  (cons (let-name exp) 
                               (map let-bind-var (let-bind-exps exp)))
                         (let-body exp))
          (cons (let-name exp) (map let-bind-exp (let-bind-exps exp)))))
      (expand-let (let-bind-exps exp) (let-body exp))))
; transform to following form
; ((lambda (var1 ... varN) body) exp1 ... expN)
(define (expand-let bind-exps body)
  (cons (make-lambda (map let-bind-var bind-exps) body)
        (map let-bind-exp bind-exps)))
(define (make-let bind-exps body)
  (list 'let bind-exps body))

;; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  ; (display (list 'let*->nested-lets exp ))(newline)
  (let ((bind-exps (let-bind-exps exp)))
    (if (null? (cdr bind-exps))
        (let->combination exp)
        (make-let
          (list (car bind-exps))
          (let*->nested-lets
            (make-let* (cdr bind-exps) (car (let-body exp))))))))
(define (make-let* bind-exps body)
  (list 'let* bind-exps body))

;; for form (for <var> <sequences> <expressions>)
;; -> 
;; (begin
;;  (define (loop seqs) 
;;    (if (not (null? seqs))
;;        (let ((<var> (car seqs)))
;;             (begin
;;               <expressions>
;;               (loop (cdr seqs))))))
;;  (loop <sequences>))
(define (for? exp) (tagged-list? exp 'for))
(define (for-var exp) (cadr exp))
(define (for-seqs exp) (caddr exp))
(define (for-exps exp) (cadddr exp))
(define (for->recur exp)
  ; (display (list 'for->recur exp))(newline)
  (sequence->exp 
    (list 
      (list 'define 
            (list 'loop 'seqs)
            (make-if (list 'not (list 'null? 'seqs))    ; Here assume we have not & null? procedures
                     (sequence->exp
                       (list (make-let (list (list (for-var exp) '(car seqs)))
                                       (for-exps exp))
                             (list 'loop (list 'cdr 'seqs))))
                     '()))
      (list 'loop (for-seqs exp)))))

;; ======== Setup Global environment ==========
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))


;; ======== Driver loop for I/O ========
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline)(newline)(display string)(newline))
(define (announce-output string)
  (newline)(display string)(newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;; ======== Start the evaluator ==========
(define the-global-environment (setup-environment))
(driver-loop)

