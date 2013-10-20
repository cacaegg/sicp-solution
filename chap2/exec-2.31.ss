(define (tree-map transformer tree)
 (cond ((null? tree) '())
       ((not (pair? tree)) (transformer tree))
       (else
        (cons (tree-map transformer (car tree))
              (tree-map transformer (cdr tree))))))
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(square-tree (list 1 (list 2 3) 4 (list 5 6 (list 7) 8) 9))
