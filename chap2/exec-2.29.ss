(define (make-mobile left right)
 (list left right))
(define (make-branch length structure)
 (list length structure))

(define (left-branch mobile)
 (car mobile))
(define (right-branch mobile)
 (cadr mobile))

(define (branch-length branch)
 (car branch))
(define (branch-structure branch)
 (cadr branch))
(define (branch? n) (pair? n))

(define (branch-weight b) 
 (+ (branch-length b)
    (let ((s (branch-structure b)))
         (if (branch? s)
             (branch-weight s)
             s))))

(define (total-weigh mobile)
 (+ (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))))

(define (torque branch)
 (* (branch-length branch)
    (branch-weight branch)))

(define (branch-balanced? b)
 (let ((s (branch-structure b)))
  (or
   (not (branch? s))
   (mobile-balanced? s))))

(define (mobile-balanced? mobile)
 (let ((l (left-branch mobile))
       (r (right-branch mobile)))
  (and
   (= (torque l)
      (torque r))
   (branch-balanced? l)
   (branch-balanced? r))))

(define a (make-branch 2 1))
(define b (make-branch 3 4))
(define c (make-branch 1 a))
(define m (make-mobile b c))
m
(left-branch m)
(right-branch m)
(branch-length c)
(branch-structure c)
(branch-structure b)

(total-weigh m)

m
(mobile-balanced? m)
(define x (make-branch 3 4))
(define y (make-branch 1 20))
(define m2 (make-mobile x y))
m2
(mobile-balanced? m2)

; Change representation to cons
(define (make-mobile left right)
 (cons left right))
(define (make-branch length structure)
 (cons length structure))
(define (right-branch mobile)
 (cdr mobile))
(define (branch-structure branch)
 (cdr branch))
