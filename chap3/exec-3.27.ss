(define (make-table same-key?)
 (let ((local-table (list '*table*)))
  (define (tree-key tree) (car tree))
  (define (tree-val tree) (cadr tree))
  (define (left-branch tree) (caddr tree))
  (define (right-branch tree) (cadddr tree))
  (define (leaf? tree) 
   (and (null? (left-branch))
        (null? (right-branch))))
  (define (make-tree key val left right)
   (list key val left right))
  (define (set-key! tree new-key)
   (set-car! tree new-key))
  (define (set-val! tree new-val)
   (set-car! (cdr tree) new-val))
  (define (set-left-branch! tree new-tree)
   (set-car! (cddr tree) new-tree))
  (define (set-right-branch! tree new-tree)
   (set-car! (cdddr tree) new-tree))

  (define (lookup cur-tree key)
   (cond ((null? cur-tree) #f)
         ((same-key? key (tree-key cur-tree))
          (tree-val cur-tree))
         ((> key (tree-key cur-tree))
          (lookup (right-branch cur-tree) key))
         ((< key (tree-key cur-tree))
          (lookup (left-branch cur-tree) key))
         (else
          (error 'lookup "ERROR when do lookup" cur-tree key))))
  (define (insert! cur-tree key value)
   (cond ((same-key? key (tree-key cur-tree))
          (set-val! cur-tree value)
          'ok)
         ((> key (tree-key cur-tree))
          (if (null? (right-branch cur-tree))
              (set-right-branch! cur-tree
               (make-tree key value '() '()))
              (insert! (right-branch cur-tree) key value))
          'ok)
         (else
           (if (null? (left-branch cur-tree))
               (set-left-branch! cur-tree
                (make-tree key value '() '()))
               (insert! (left-branch cur-tree) key value))
           'ok)))
  (define (dispatch m)
   (cond ((eq? m 'lookup-proc) 
          (lambda (k) (lookup (cdr local-table) k)))
         ((eq? m 'insert-proc!) 
          (lambda (k v) 
           (if (null? (cdr local-table))
               (begin
                (set-cdr! local-table (make-tree k v '() '()))
                'ok)
               (insert! (cdr local-table) k v))))
         ((eq? m 'print) (lambda () (display local-table)(newline)))
         (else (error 'dispatch "Unknown operation -- TABLE" m))))
  dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print))

(define (memorize f)
 (let ((table (make-table eq?)))
  (lambda (x)
   (let ((previously-computed-result ((table 'lookup-proc) x)))
    (or previously-computed-result
        (let ((result (f x)))
         ((table 'insert-proc!) x result)
         result))))))

(define memo-fib
 (memorize (lambda (n)
            (cond ((= n 0) 0)
                  ((= n 1) 1)
                  (else
                   (+ (memo-fib (- n 1))
                      (memo-fib (- n 2))))))))
(define (fib n)
 (cond ((= n 0) 0)
       ((= n 1) 1)
       (else
        (+ (fib (- n 1))
           (fib (- n 2))))))
(define memo-fib-2 (memorize fib))

(memo-fib 120)
(memo-fib-2 35)
; memo-fib-2 will not work the memorize technique.
; Since it use fib to recursive itself, the rest of 
; computing will not utilize the memorized result.


; |--------------------------------------------------------
; | make-table:...                  memo-fib:
; | memorize:                                \
; |          \                               |
; |          |                               |
; |__________|_______________________________|____________
;            |                               |
;            |                               |                |---------------------|
;            v                               v                | f(lambda):-----|    |
;           O O                             O O ------------> | x: 3           |    |
;        parameter: f                     parameter: x        | table:..       |    |
;        body:...                         body: (let ((pre..  |________________|____|
;                                                               ^              |
;                                |------------------|           |              v
;                                | 0: 0             |___________|   parameter: n
;                                | 1: 1             |               body: ...
;                                |                  |
;                                | table-frame      |
;                                |__________________|
;
