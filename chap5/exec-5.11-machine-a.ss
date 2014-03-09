(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;;; Register implementation and operation interface
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    (else
	     (error 'register "Unknown request" message))))
    dispatch))
(define (get-contents register)
  (register 'get))
(define (set-contents! register val)
  ((register 'set) val))

;;; Stack implementation and operation interface
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
	  (error 'pop "Empty stack")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    (define (initialize)
      (set! s '()))
    (define (dispatch msg)
      (cond ((eq? msg 'push) push)
	    ((eq? msg 'pop) (pop))
	    ((eq? msg 'initialize) (initialize))
	    (else
	     (error 'stack "Unknown request" msg))))
    dispatch))
(define (pop stack) (stack 'pop))
(define (push stack val)
  ((stack 'push) val))

(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value))
(define (get-register machine register-name)
  ((machine 'get-register) register-name))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error 'allocate-register "Duplicated register name" name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error 'lookup-register "Unknown register" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch msg)
	(cond ((eq? msg 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? msg 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? msg 'allocate-register) allocate-register)
	      ((eq? msg 'get-register) lookup-register)
	      ((eq? msg 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? msg 'stack) stack)
	      ((eq? msg 'operations) the-ops)
	      (else (error 'machine "Unknown request" msg))))
      dispatch)))

;; The Assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
	 (let ((next-inst (car text)))
	   (if (symbol? next-inst)
	       (receive insts
			(cons (make-label-entry next-inst
						insts)
			      labels))
	       (receive (cons (make-instruction next-inst)
			      insts)
			labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
	inst
	(make-execution-procedure
	 (instruction-text inst) labels machine
	 pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error 'lookup-label "Undefined label" label-name))))

(define (make-execution-procedure inst labels machine
				  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	((eq? (car inst) 'inc)
	 (make-inc inst machine ops pc))
	(else error 'make-execution-procedure
	      "Unknown instruction type" inst)))

(define (make-inc inst machine operations pc)
  (let ((target
	 (get-register machine
		       (inc-register inst))))
    (lambda ()
      (set-contents! target (+ (get-contents target) 1))
      (advance-pc pc))))
(define (inc-register inst)
  (register-exp-reg (cadr inst)))

(define (make-assign inst machine labels operations pc)
  (let ((target
	 (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc
	   (if (operation-exp? value-exp)
	       (make-operation-exp
		value-exp machine labels operations)
	       (make-primitive-exp
		(car value-exp) machine labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))

(define (assign-reg-name assign-inst)
  (cadr assign-inst))
(define (assign-value-exp assign-inst)
  (cddr assign-inst))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc
	       (make-operation-exp
		condition machine labels operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error 'make-test "Bad TEST instruction" inst))))

(define (test-condition test-inst)
  (cdr test-inst))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts
	       (lookup-label labels (label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc pc))))
	(error 'make-branch "Bad BRANCH instruction" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts
		  (lookup-label labels
				(label-exp-label dest))))
	     (lambda () (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg
		  (get-register machine
				(register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error 'make-goto "Bad GOTO instruction" inst)))))

(define (goto-dest goto-inst)
  (cadr goto-inst))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operations-exp? action)
	(let ((action-proc
	       (make-operation-exp
		action machine labels operations)))
	  (lambda ()
	    (action-proc)
	    (advance-pc pc)))
	(error 'make-perform "Bad PERFORM instruction" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts
		(lookup-label labels
			      (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine
				(register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(else
	 (error 'make-primitive-procedure
		"Unknown expression type" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(make-primitive-exp e machine labels))
	      (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(cadr val)
	(error 'lookup-prim "Unknown operations" symbol))))