(define (square x) (* x x))
(define (sine number)
 (apply-generic 'sine number))
(define (cosine number)
 (apply-generic 'cosine number))
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
(define (project obj)
 (let ((type (type-tag obj)))
  (let ((projector (get 'project type)))
   (if (null? projector)
       '()
       (let ((low-obj (projector obj)))
        (if (null? low-obj)
            '()
            (let ((raised-obj ((get 'raise (type-tag low-obj)) low-obj type)))
             (if (equ? obj raised-obj) low-obj '()))))))))
(define (drop obj)
 (let ((next-obj (project obj)))
  (if (null? next-obj)
      obj
      (drop next-obj))))

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
 (define (simplify-ans ans)
  (if (or (eq? ans #t) (eq? ans #f))
      ans
      (let ((new-ans (drop ans)))
       (let ((new-type (type-tag new-ans))
             (ori-type (type-tag ans)))
        (if (eq? new-type ori-type)
            ans
            (simplify-ans new-ans))))))
 (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
   (if (not (null? proc)) 
       (simplify-ans (apply proc (map content args)))
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
 (put 'sine 'scheme-number (lambda (x) (tag (sin x))))
 (put 'cosine 'scheme-number (lambda (x) (tag (cos x))))
 (put 'raise 'scheme-number
  (lambda (obj type) 
   ((get-coercion 'scheme-number type) obj)))
 'done)

;;
;; Rational Package
;;
(define (install-rational-package)
 ;; internal procedures
 (define (floats r)
  (if (integer? r)
      0
      (+ 1 (floats (* r 10)))))
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
 (define (sine r)
  (let ((sin-r (sin (/ (* (numer r) 1.0) (denom r)))))
   (let ((num-f (floats sin-r)))
    (make-rat (* sin-r (* 10 num-f))
              (* 10 num-f)))))
 (define (cosine r)
  (let ((cos-r (cos (/ (* (numer r) 1.0) (denom r)))))
   (let ((num-f (floats cos-r)))
    (make-rat (* cos-r (* 10 num-f))
              (* 10 num-f)))))
 (define (equ-rat? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
 (define (zero-rat? r)
  (zero? (numer r)))

 ;; interfaces
 (define (tag x) (attach-tag 'rational x))
 (put 'numer 'rational numer)
 (put 'denom 'rational denom)
 (put 'add '(rational rational)
  (lambda (x y) (tag (add-rat x y))))
 (put 'sub '(rational rational)
  (lambda (x y) (tag (sub-rat x y))))
 (put 'mul '(rational rational)
  (lambda (x y) (tag (sub-rat x y))))
 (put 'div '(rational rational)
  (lambda (x y) (tag (div-rat x y))))
 (put 'sine '(rational) 
  (lambda (r) (tag (sine r))))
 (put 'cosine '(rational)
  (lambda (r) (tag (cosine r))))
 (put 'make 'rational
  (lambda (n d) (tag (make-rat n d))))
 (put 'rational? 'rational
  (lambda (obj) (eq? 'rational (type-tag obj))))
 (put 'equ? '(rational rational)
  (lambda (x y) (equ-rat? x y)))
 (put 'zero? '(rational)
  (lambda (r) (zero-rat? r)))
 (put 'raise 'rational
  (lambda (obj type) 
   ((get-coercion 'rational type) obj)))
 (put 'project 'rational
  (lambda (obj) 
   (make-scheme-number
    (round (/ (numer (content obj))
              (denom (content obj)))))))
 (put-coercion 'scheme-number 'rational
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
   (sqrt (add (square (real-part z))
            (square (imag-part z)))))
 (define (angle z)
   (arctan (imag-part z) (real-part z)))
 (define (make-from-mag-ang r a)
   (consine (mul r (cosine a)) (mul r (sine a))))

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
  (mul (magnitude z) (consine (angle z))))
 (define (imag-part z)
  (mul (magnitude z) (sine (angle z))))
 (define (make-from-real-imag x y)
  (cons (sqrt (add (square x) (square y)))
        (arctan y x)))

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
  (make-from-real-imag (add (real-part z1) (real-part z2))
                       (add (imag-part z1) (imag-part z2))))
 (define (sub-complex z1 z2)
  (make-from-real-imag (sub (real-part z1) (real-part z2))
                       (sub (imag-part z1) (imag-part z2))))
 (define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))
 (define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))
 (define (equ-complex? z1 z2)
  (and (equ? (real-part z1) (real-part z2))
       (equ? (imag-part z1) (imag-part z2))))
 (define (zero-complex? com)
  (and (zero? (real-part com))
       (zero? (imag-part com))))

 ;; Coercion procedures
 (define (scheme-number->complex n)
  (make-complex-from-real-imag (content n) 0))
 (define (rational->complex r)
  (make-complex-from-real-imag r 0))

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
 (put 'project 'complex
      (lambda (obj) 
       (let ((r (real-part obj)))
        (if ((get 'rational? 'rational) r)
            r
            (make-rational r 1)))))
 (put-coercion 'scheme-number 'complex scheme-number->complex)
 (put-coercion 'rational 'complex rational->complex)
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

(let ((x (make-rational 4 2)))
 (drop x))

(let ((x (make-rational 5 2)))
 (drop x))

(let ((x (make-complex-from-real-imag 5 2)))
 (drop x))

(let ((x (make-complex-from-real-imag 5 0)))
 (drop x))

(let ((z1 (make-complex-from-real-imag 5 2))
      (z2 (make-complex-from-real-imag 1 1)))
 (sub z1 z2))

(let ((z1 (make-complex-from-real-imag 5 2))
      (z2 (make-complex-from-real-imag 1 2)))
 (sub z1 z2))

(let ((r1 (make-rational 1 2)))
 (let ((z1 (make-complex-from-real-imag r1 2))
       (z2 (make-complex-from-real-imag 1 2)))
  (add z1 z2)))
