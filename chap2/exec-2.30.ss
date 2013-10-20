(define (square x) (* x x))
(define (square-tree tree)
 (cond ((null? tree) '())
       ((not (pair? tree)) (square tree))
       (else
        (cons (square-tree (car tree))
              (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 3) 4 (list 5 6 (list 7) 8) 9))

(define (square-tree-m tree)
 (map (lambda (subtree)
       (if (pair? subtree)
           (square-tree subtree)
           (square subtree)))
      tree))

(square-tree-m (list 1 (list 2 3) 4 (list 5 6 (list 7) 8) 9))
