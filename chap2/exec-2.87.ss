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
(define (apply-generic op . args)
 (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
   (if (not (null? proc))
       (apply proc (map content args))
       (error 'apply-generic "No method for these types -- APPLY-GENERIC"
              (list op type-tags))))))

;;
;; Orinary Arithmetic Package
;;
(define (install-scheme-number-package)
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
 (put '=zero? '(scheme-number)
  (lambda (x) (zero? x)))
 (put 'make 'scheme-number
  (lambda (x) (tag x)))
 'done)

;; The polynomial package
(define (install-polynomial-package)
 ;; Representation of poly
 (define (make-poly variable term-list)
  (cons variable term-list))
 (define (variable poly) (car poly))
 (define (term-list poly) (cdr poly))
 (define (variable? x) (symbol? x))
 (define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

 ;; Representation of terms, and term-list
 (define (make-termlist t1 . tn)
  (append (list t1) tn))
 (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
 (define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
          (cond ((> (order t1) (order t2))
                 (adjoin-term
                  t1 (add-terms (rest-terms L1) L2)))
                ((< (order t1) (order t2))
                 (adjoin-term
                  t2 (add-terms L1 (rest-terms L2))))
                (else
                 (adjoin-term
                  (make-term (order t1)
                             (add (coeff t1) (coeff t2)))
                  (add-terms (rest-terms L1)
                             (rest-terms L2)))))))))
 (define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
 (define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-term-list)
      (let ((t2 (first-term L)))
       (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (make-term-by-all-terms t1 (rest-terms L))))))

 (define (the-empty-termlist) '())
 (define (first-term term-list) (car term-list))
 (define (rest-terms term-list) (cdr term-list))
 (define (empty-termlist? term-list) (null? term-list))

 (define (make-term order coeff) (list order coeff))
 (define (order term) (car term))
 (define (coeff term) (cadr term))

 ;; Polynomial arithmetic
 (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error 'add-poly "Poly not in same var -- ADD-POLY" (list p1 p2))))
 (define (mul-poly p1 p2)
  (if (smae-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error 'mul-poly "Poly not in same var -- MUL-POLY" (list p1 p2))))

 (define (tag p) (attach-tag 'polynomial p))
 (put 'add '(polynomial polynomial)
  (lambda (p1 p2) (tag (add-poly p1 p2))))
 (put 'mul '(polynomial polynomial)
  (lambda (p1 p2) (tag (mul-poly p1 p2))))
 (put 'make 'polynomial
  (lambda (var terms) (tag (make-poly var terms))))
 (put 'make 'term make-term)
 (put 'make 'termlist make-termlist)
 'done)

; Predicates
(define (=zero? number) (apply-generic '=zero? number))

(install-scheme-number-package)
(install-polynomial-package)

(define (make-polynomial var terms)
 ((get 'make 'polynomial) var terms))
(define (make-term order coeff)
 ((get 'make 'term) order coeff))
(define (make-termlist t1 t2)
 ((get 'make 'termlist) t1 t2))
(define (add p1 p2)
 (apply-generic 'add p1 p2))

(let ((t1 (make-term 1 2))
      (t2 (make-term 4 5))
      (t3 (make-term 1 3))
      (t4 (make-term 100 2)))
 (let ((tls-1 (make-termlist t2 t1))
       (tls-2 (make-termlist t4 t3)))
  (let ((p1 (make-polynomial 'x tls-1))
        (p2 (make-polynomial 'x tls-2)))
   (add p1 p2))))
