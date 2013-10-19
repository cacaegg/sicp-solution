(define (reverse ls)
 (let ((next (cdr ls)))
  (if (null? next)
      (list (car ls))
      (append (reverse next) 
              (list (car ls))))))

(define (deep-reverse ls)
 (cond ((null? ls) '())
       ((not (pair? ls)) ls)
       (else (append (deep-reverse (cdr ls))
                     (list (deep-reverse (car ls)))))))

(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)
(newline)

(define y (list 1 (list 2 3)))
y
(reverse y)
(deep-reverse y)
(newline)

(define z (list (list 1 2) 3))
z
(reverse z)
(deep-reverse z)
(newline)

(define a (list 1 (list 2 3) 4))
a
(reverse a)
(deep-reverse a)
