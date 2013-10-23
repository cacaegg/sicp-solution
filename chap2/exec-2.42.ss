(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
 (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
 (cond ((> low high) '())
       (else (cons low (enumerate-interval (1+ low) high)))))

(define empty-board '())

(define (adjoin-position new-row k rest)
 (cons new-row rest))

(define (in? number seq)
 (let ((result (find (lambda (x) (= x number)) seq)))
  (cond ((eq? result #f) result)
        (else #t))))

(define (safe? k positions)
 (define (row-col-ok? positions)
  (let ((cur-queen (car positions)))
       (not (in? cur-queen (cdr positions)))))
 (define (diagonal-ok? positions)
  (define (direction-ok? cur-queen op positions) 
   (cond ((null? positions) #t)
         ((= (op cur-queen) (car positions)) #f)
         (else (direction-ok? (op cur-queen) op (cdr positions)))))
  (let ((cur-queen (car positions)))
    (if (null? (cdr positions))
        #t
        (and (direction-ok? cur-queen 1- (cdr positions))       ; Check until left top
             (direction-ok? cur-queen 1+ (cdr positions))))))   ; Check until right top
 (and (row-col-ok?  positions)
      (diagonal-ok? positions)))

(define (queens board-size)
 (define (queen-cols k)
  (if (= k 0)
      (list empty-board)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
         (map (lambda (new-row)
               (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 board-size)))
        (queen-cols (- k 1))))))
 (queen-cols board-size))

(queens 8)
