(define (square x) (* x x))
(define (zip proc-ls ls)
 (define (iter p-ls ls result)
  (if (or (null? p-ls) (null? ls))
      result
      (iter (cdr p-ls) (cdr ls)
            (append result (list ((car p-ls) (car ls)))))))
 (iter proc-ls ls '()))

;; Operation, type -> procedure
;; ;; Dispatch table.
(define *op-table* (make-hashtable equal-hash equal?))
(define (put op type proc)
 (hashtable-set! *op-table* (list op type) proc))
(define (get op type)
 (hashtable-ref *op-table* (list op type) '()))

(define (attach-tag tag obj) 
 (cond ((number? obj) obj)
       (else (cons tag obj))))
(define (type-tag obj) 
 (cond ((number? obj) 'scheme-number)
       ((pair? obj) (car obj))
       (else
        (error 'type-tag "Bad tagged datum -- TYPE-TAG" obj))))
(define (content obj)
 (cond ((number? obj) obj)
       ((pair? obj) (cdr obj))
       (else
        (error 'content "Bad tagged datum -- CONTENT" obj))))
(define (raise obj target-type)
 (if (eq? (type-tag obj) target-type)
     obj
     ((get 'raise (type-tag obj)) obj target-type)))


;; Coercion, type -> other types
;; ;; Coercion table.
(define *coer-table* (make-hashtable equal-hash equal?))
(define (put-coercion type1 type2 proc)
 (hashtable-set! *coer-table* (list type1 type2) proc))
(define (get-coercion type1 type2)
 (hashtable-ref *coer-table* (list type1 type2) '()))

(define (apply-generic op . args)
 (define (higher-type? type1 type2)
  (let ((proc (get-coercion type2 type1)))
   (if (null? proc) #f #t)))
 ;; Try to coercion all-args to target-typea
 (define (coer-all remain-args target-type result) 
  (if (null? remain-args)
      result
      (let ((cur-obj (car remain-args)))
       (coer-all (cdr remain-args) target-type 
                 (append result (list (raise cur-obj target-type)))))))
 ;; Find the highest argument type in the tower
 (define (find-highest type-list cur-type)
  (if (null? type-list)
      cur-type
      (let ((compared-type (car type-list)))
       (if (higher-type? compared-type cur-type)
           (find-highest (cdr type-list) compared-type)
           (find-highest (cdr type-list) cur-type)))))
 (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
   (if (not (null? proc)) 
       (apply proc (map content args))
       (let ((highest-type (find-highest (cdr type-tags) (car type-tags))))
        (apply apply-generic 
         (append (list op) (coer-all args highest-type '()))))))))
             
;;
;; Orinary Arithmetic Package
;;
(define (install-scheme-number-package)
 ;; internal procedures

 (define (tag x)
  (attach-tag 'scheme-number x))
 (put 'add '(scheme-number scheme-number)
  (lambda (x y) (tag (+ x y))))
 (put 'sub '(scheme-number scheme-number)
  (lambda (x y) (tag (- x y))))
 (put 'mul '(scheme-number scheme-number)
  (lambda (x y) (tag (* x y))))
 (put 'div '(scheme-number scheme-number)
  (lambda (x y) (tag (/ x y))))
 (put 'equ? '(scheme-number scheme-number)
  (lambda (x y) (= x y)))
 (put 'zero? '(scheme-number)
  (lambda (x) (zero? x)))
 (put 'make 'scheme-number
  (lambda (x) (tag x)))
 (put 'exp '(scheme-number scheme-number)
  (lambda (x y) (tag (expt x y)))) ;; Use primitive expt
 (put 'raise 'scheme-number
  (lambda (obj type) 
   ((get-coercion 'scheme-number type) obj)))
 'done)

;;
;; Rational Package
;;
(define (install-rational-package)
 ;; internal procedures
 (define (numer x) (car x))
 (define (denom x) (cdr x))
 (define (make-rat n d)
  (let ((g (gcd n d)))
   (cons (/ n g) (/ d g))))
 (define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
 (define (sub-rat x y)
  (make-rat (- (* (numer x) (denum y))
               (* (numer y) (denum x)))
            (* (denom x) (denum y))))
 (define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
 (define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
 (define (equ-rat? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
 (define (zero-rat? r)
  (zero? (numer r)))

 ;; interfaces
 (define (tag x) (attach-tag 'rational x))
 (put 'add '(rational rational)
  (lambda (x y) (tag (add-rat x y))))
 (put 'sub '(rational rational)
  (lambda (x y) (tag (sub-rat x y))))
 (put 'mul '(rational rational)
  (lambda (x y) (tag (sub-rat x y))))
 (put 'div '(rational rational)
  (lambda (x y) (tag (div-rat x y))))
 (put 'make 'rational
  (lambda (n d) (tag (make-rat n d))))
 (put 'equ? '(rational rational)
  (lambda (x y) (equ-rat? x y)))
 (put 'zero? '(rational)
  (lambda (r) (zero-rat? r)))
 (put 'raise 'rational
  (lambda (obj type) 
   ((get-coercion 'rational type) obj)))
 (put-coercion '(scheme-number rational-number)
  (lambda (n) ((get 'make 'rational) (content n) 1)))
 'done)

;;
;; Complex Package
;;
(define (install-rectangular-package)
 ;; internal procedures
 (define (real-part z) (car z))
 (define (imag-part z) (cdr z))
 (define (make-from-real-imag x y) (cons x y))
 (define (magnitude z)
   (sqrt (+ (square (real-part z))
            (square (imag-part z)))))
 (define (angle z)
   (atan (imag-part z) (real-part z)))
 (define (make-from-mag-ang r a)
   (cons (* r (cos a)) (* r (sin a))))
 (define (complex->complex z) z)

 ;; interface to the rest of the system
 (define (tag x) (attach-tag 'rectangular x))
 (put 'real-part '(rectangular) real-part)
 (put 'imag-part '(rectangular) imag-part)
 (put 'magnitude '(rectangular) magnitude)
 (put 'angle '(rectangular) angle)
 (put 'make-from-real-imag 'rectangular
      (lambda (x y) (tag (make-from-real-imag x y))))
 (put 'make-from-mag-ang 'rectangular
      (lambda (r a) (tag (make-from-mag-ang r a))))
 'done)

(define (install-polar-package)
 ;; internal procedures
 (define (magnitude z) (car z))
 (define (angle z) (cdr z))
 (define (make-from-mag-ang r a) (cons r a))
 (define (real-part z)
  (* (magnitude z) (cons (angle z))))
 (define (imag-part z)
  (* (magnitude z) (sin (angle z))))
 (define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

 ;; interfaces
 (define (tag x) (attach-tag 'polar x))
 (put 'real-part '(polar) real-part)
 (put 'imag-part '(polar) imag-part)
 (put 'magnitude '(polar) magnitude)
 (put 'angle '(polar) angle)
 (put 'make-from-real-imag 'polar
      (lambda (x y) (tag (make-from-real-imag x y))))
 (put 'make-from-mag-ang 'polar
      (lambda (r a) (tag (make-from-mag-ang r a))))
 'done)

(define (install-complex-package)
 ;; imported procedures from rectangular and polar packages
 (define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
 (define (make-from-man-ang r a)
  ((get 'make-from-man-ang 'polar) r a))
 (define (real-part z) (apply-generic 'real-part z))
 (define (imag-part z) (apply-generic 'imag-part z))
 (define (magnitude z) (apply-generic 'magnitude z))
 (define (angle z) (apply-generic 'angle z))

 ;; internal procedures
 (define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
 (define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
 (define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
 (define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
 (define (equ-complex? z1 z2)
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))))
 (define (zero-complex? com)
  (and (zero? (real-part com))
       (zero? (imag-part com))))

 ;; Coercion procedures
 (define (scheme-number->complex n)
  (make-complex-from-real-imag (content n) 0))

 ;; interface to rest of the system
 (define (tag z) (attach-tag 'complex z))
 (put 'add '(complex complex)
      (lambda (z1 z2) (tag (add-complex z1 z2))))
 (put 'sub '(complex complex)
      (lambda (z1 z2) (tag (sub-complex z1 z2))))
 (put 'mul '(complex complex)
      (lambda (z1 z2) (tag (mul-complex z1 z2))))
 (put 'div '(complex complex)
      (lambda (z1 z2) (tag (div-complex z1 z2))))
 (put 'real-part '(complex) real-part)
 (put 'imag-part '(complex) imag-part)
 (put 'magnitude '(complex) magnitude)
 (put 'angle '(complex) angle)
 (put 'make-from-real-imag 'complex
      (lambda (x y) (tag (make-from-real-imag x y))))
 (put 'make-from-mag-ang 'complex
      (lambda (x y) (tag (make-from-mag-ang x y))))
 (put 'equ? '(complex complex)
      (lambda (x y) (equ-complex? x y)))
 (put 'zero? '(complex)
      (lambda (com) (zero-complex? com)))
 (put 'add-3 '(complex complex complex)
      (lambda (x y z) (tag (add-complex x (add-complex y z)))))
 (put-coercion 'scheme-number 'complex scheme-number->complex)
 (put-coercion 'rational 'complex
  (lambda (r) (make-complex-from-real-imag (content r) 0)))
 'done)

;; Interfaces of General Arithmetic Package
; Constructor
(define (make-scheme-number n)
 ((get 'make 'scheme-number) n))
(define (make-rational n d)
 ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
 ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
 ((get 'make-from-mag-ang 'complex) r a))

; Ordinary arithmetic
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (add-3 x y z) (apply-generic 'add-3 x y z))

; Complex arithmetic
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic ' angle z))

; Predicates
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? number) (apply-generic 'zero? number))

; Install all the packages
(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package) 
(install-complex-package)

(let ((x (make-complex-from-real-imag 1 2))
      (y (make-scheme-number 3))
      (z (make-scheme-number 5)))
 (add-3 x y z))
