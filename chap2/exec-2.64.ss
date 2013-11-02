(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
 (list entry left right))

(define (element-of-set? x set)
 (cond ((null? set) #f)
       ((= x (entry set)) #t)
       ((< x (entry set)) 
        (element-of-set? x (left-branch set)))
       ((> x (entry set))
        (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
 (cond ((null? set) (make-tree x '() '()))
       ((= x (entry set)) set)
       ((< x (entry set))
        (make-tree (entry set)
                   (adjoin-set x (left-branch set))
                   (right-branch set)))
       ((> x (entry set))
        (make-tree (entry set)
                   (left-branch set)
                   (adjoin-set x (right-branch set))))))
(define (list->tree elements)
 (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
 (if (= n 0)
     (cons '() elts)
     (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
       (let ((left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1))))
        (let ((this-entry (car non-left-elts))
              (right-result (partial-tree (cdr non-left-elts)
                                          right-size)))
         (let ((right-tree (car right-result))
               (remaining-elts (cdr right-result)))
          (cons (make-tree this-entry left-tree right-tree)
                remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))

; a. It first divide (n - 1) by 2, to get equal size of left & right.
;    Then recursive itself to construct left tree result with n/2.
;    Third, grab the non-left-elements, calculate right tree size, 
;    which is n - (left-size + 1), 1 for entry point. Then construct 
;    the right tree with itself. 
;    The whole recursive is terminated at n equals zero.
;                5
;              /   \
;             1     9
;              \   / \
;               3 7  11
;
; b. To construct left tree, n / 2
;    To construct right tree, n / 2
;    The order of growth is O(n)
