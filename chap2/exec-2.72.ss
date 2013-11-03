(define (make-leaf symbol weight)
 (list 'leaf symbol weight))
(define (leaf? object)
 (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
 (list left
       right
       (append (symbols left) (symbols right))
       (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
 (if (leaf? tree)
     (list (symbol-leaf tree))
     (caddr tree)))
(define (weight tree)
 (if (leaf? tree)
     (weight-leaf tree)
     (cadddr tree)))

(define (decode bits tree)
 (define (decode-1 bits current-branch)
  (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
       (if (leaf? next-branch)
           (cons (symbol-leaf next-branch)
                 (decode-1 (cdr bits) tree))
           (decode-1 (cdr bits) next-branch)))))
 (decode-1 bits tree))
(define (choose-branch bit branch)
 (cond ((= bit 0) (left-branch branch))
       ((= bit 1) (right-branch branch))
       (else (error "bad bit -- CHOOSE BRANCH" bit)))) 
(define (encode message tree)
 (if (null? message)
     '()
     (append (encode-symbol (car message) tree)
             (encode (cdr message tree) tree))))
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

; To encode a symbol, 
; for worst case need n steps to finish the tree, 
; while each level,
;   memq takes n
;   left-symbols takes log n
;   right-symbols takes log n
; Hence, it's O(n * n) to encode a symbol
;
; In case of special case, Most frequent (only spend 1 level) is O(n),
; And Least frequent (spend n levels) is O(n * n)
