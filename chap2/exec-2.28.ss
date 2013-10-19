(define (fringe tree)
 (cond ((null? tree) '())
       ((pair? tree) (append (fringe (car tree))
                             (fringe (cdr tree))))
       (else (list tree))))

(define x (list (list 1 2) (list 3 4)))
x
(fringe x)
(fringe (list x x))

(define y (list 1 (list 2 3)))
y
(fringe y)

(define z (list 1 (list 2 (list 3 4) 5 6) 7))
z
(fringe z)
