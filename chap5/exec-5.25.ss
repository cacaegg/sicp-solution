(load "syntax.ss")
(load "support-eceval.ss")
(load "regmachine.ss")

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define prim-operations
  (list (list 'read read)

	;; operation in syntax.ss
	(list 'self-evaluating? self-evaluating?)
	(list 'variable? variable?)
	(list 'quoted? quoted?)
	(list 'assignment? assignment?)
	(list 'definition? definition?)
	(list 'if? if?)
	(list 'lambda? lambda?)
	(list 'begin? begin?)
	(list 'application? application?)
	(list 'true? true?)

	;; syxtax processing
	(list 'lookup-variable-value lookup-variable-value)
	(list 'text-of-quotation text-of-quotation)
	(list 'operator operator)
	(list 'operands operands)
	(list 'empty-arglist empty-arglist)
	(list 'no-operands? no-operands?)
	(list 'first-operand first-operand)
	(list 'last-operand? last-operand?)
	(list 'adjoin-arg adjoin-arg)
	(list 'rest-operands rest-operands)
	(list 'begin-actions begin-actions)
	(list 'first-exp first-exp)
	(list 'last-exp? last-exp?)
	(list 'rest-exps rest-exps)
	(list 'if-predicate if-predicate)
	(list 'if-consequent if-consequent)
	(list 'if-alternative if-alternative)
	(list 'assignment-variable assignment-variable)
	(list 'assignment-value assignment-value)
	(list 'set-variable-value! set-variable-value!)
	(list 'definition-variable definition-variable)
	(list 'definition-value definition-value)
	(list 'define-variable! define-variable!)
	(list 'lambda-parameters lambda-parameters)
	(list 'lambda-body lambda-body)
	(list 'make-procedure make-procedure)

	;; for lazy evaluator
	(list 'delay-it delay-it)
	(list 'thunk? thunk?)
	(list 'thunk-exp thunk-exp)
	(list 'thunk-env thunk-env)
	
	;; apply processing
	(list 'primitive-procedure? primitive-procedure?)
	(list 'compound-procedure? compound-procedure?)
  	))

(define eceval
  (make-machine
   '(exp env val continue proc argl unev)
   prim-operations
   '(
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))
    
    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure)
	    (reg unev) (reg exp) (reg env))
    (goto (reg continue))
    ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label actual-value))
    ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (test (op primitive-procedure?) (reg proc))
    (branch (label force-it))
    (assign val (op delay-it) (reg exp))
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (test (op primitive-procedure?) (reg proc))
    (branch (label force-it))
    (assign val (op delay-it) (reg exp))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))
    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))
    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    ;; Tail recursion, don't save and restore the unneccessary register
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))
    ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))
    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))
    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assignment val (const ok))
    (goto (reg continue))
    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (assignment exp (op definition-value) (reg exp))
    (save unev)
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assignment val (const ok))
    (goto (reg continue))
    
    unknown-expression-type

    primitive-apply
    (assign val
	    (op apply-primitive-procedure)
	    (reg proc)
	    (reg argl))
    (restore continue)
    (goto (reg continue))
    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
	    (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))
    unknown-procedure-type

    ;; for lazy evaluator
    actual-value
    (save continue)
    (assign continue (label actual-value-do-force))
    (goto (label eval-dispatch))
    actual-value-do-force
    (restore continue)
    (assign exp (reg val))
    (goto (label force-it))
    
    force-it
    (test (op thunk?) (reg exp))
    (branch (label force-it-obj))
    (save env)
    (save continue)
    (assign env (op thunk-env) (reg exp))
    (assign exp (op thunk-exp) (reg exp))
    (assign continue (label force-it-act))
    (goto (label eval-dispatch))
    force-it-act
    (restore continue)
    (restore env)
    (goto (reg continue))
    force-it-obj
    (assign val (reg exp))
    (goto (reg continue))
    )))
