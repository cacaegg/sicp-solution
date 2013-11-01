(define (element-of-set? x set)
 (cond ((null? set) #f)
       ((equal? x (car set)) #t)    ; equal? Set element need not to be symbol
       ((< x (car set)) #f)
       (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
 (define (iter set result)
  (cond ((null? set) (append result (list x)))
        ((< x (car set)) 
         (append result (cons x set)))
        ((> x (car set))
         (iter (cdr set)
               (append result (list (car set)))))
        ((= x (car set))
         (append result set))))
 (iter set '()))

(define (intersection-set set1 set2)
 (if (or (null? set1) (null? set2))
     '()
     (let ((x1 (car set1)) (x2 (car set2)))
       (cond ((= x1 x2)
              (cons x1 (intersection-set (cdr set1) (cdr set2))))
             ((< x1 x2)
              (intersection-set (cdr set1) set2))
             ((> x1 x2)
              (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
 (if (null? set1) 
     set2
     (union-set (cdr set1)
                (adjoin-set (car set1) set2))))

(define empty-set '())

(element-of-set? 1 empty-set)
(adjoin-set -1 (list 1 2 3))
(adjoin-set 2 (list 1 2 3))
(adjoin-set 4 (list 1 2 3))

(let ((set1 (adjoin-set 1 (adjoin-set 2 empty-set)))
      (set2 (adjoin-set 2 (adjoin-set 3 empty-set))))
     (intersection-set set1 set2))

(let ((set1 (adjoin-set 1 (adjoin-set 2 empty-set)))
      (set2 (adjoin-set 2 (adjoin-set 3 empty-set))))
     (union-set set1 set2))
