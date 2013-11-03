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

(define (encode-symbol symbol tree)
 (cond ((eq? #f (memq symbol (symbols tree)))
        (error symbol "No such symbol"))
       ((leaf? tree)
        '())
       (else 
        (let ((left-symbols (symbols (left-branch tree)))
              (right-symbols (symbols (right-branch tree))))
         (if (eq? #f (memq symbol left-symbols))
             (cons 1 (encode-symbol symbol (right-branch tree)))
             (cons 0 (encode-symbol symbol (left-branch tree))))))))
(define (encode-symbols symbols tree)
 (if (null? symbols)
     '()
     (append (encode-symbol (car symbols) tree)
             (encode-symbols (cdr symbols) tree))))

(define n5
 '((a 1) (b 2) (c 4) (d 8) (e 16)))

(define n10
 '((a 1) (b 2) (c 4) (d 8) (e 16)
   (f 32) (g 64) (h 128) (i 256) (j 512)))

(define (generate-huffman-tree pairs)
 (successive-merge (make-leaf-set pairs)))

(newline)

(generate-huffman-tree n5)

(generate-huffman-tree n10)

; Most Frequent > 1 bit
; Least Frequent > n - 1 bits
