(define (make-leaf symbol weight)
 (list 'leaf symbol weight))
(define (leaf? object)
 (eq? (car object) 'leaf))
(define (tree? object)
 (eq? (car object) 'tree))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
 (list 'tree
       left
       right
       (append (symbols left) (symbols right))
       (+ (weight left) (weight right))))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (symbols tree)
 (if (leaf? tree)
     (list (symbol-leaf tree))
     (cadddr tree)))
(define (weight tree)
 (if (leaf? tree)
     (weight-leaf tree)
     (car (cddddr tree))))
(define (adjoin-set x set)
 (cond ((null? set) (list x))
       ((< (weight x) (weight (car set))) (cons x set))
       (else (cons (car set)
                   (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
 (if (null? pairs)
     '()
     (let ((pair (car pairs)))
      (adjoin-set (if (or (leaf? pair) (tree? pair)) ; To sort it, we need to know if it's  
                      pair                           ; tree node or leaf
                      (make-leaf (car pair) (cadr pair)))
                  (make-leaf-set (cdr pairs))))))
(define (successive-merge o-pairs)
 (if (= 1 (length o-pairs)) 
     (car o-pairs)
     (successive-merge ; Resort again for each merge
      (make-leaf-set 
       (cons (make-code-tree (car o-pairs)
                             (cadr o-pairs))
             (cddr o-pairs))))))

(define message '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(define (generate-huffman-tree pairs)
 (successive-merge (make-leaf-set pairs)))
(generate-huffman-tree message)
