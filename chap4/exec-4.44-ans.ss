(define (require p)
  (if (not p) (amb)))

(define (xor-require p1 p2)
  (cond ((and p1 p2) (amb))
        ((and (not p1) (not p2)) (amb))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items))
         false)
        (else (distinct? (cdr items)))))

(define (solve-8-queen)
  (define (in-x? target orig delta)
    (cond ((or (<= orig 0) (> orig 8)) false)
          ((or (= target (+ orig delta))
               (= target (- orig delta))) true)
          (else false)))
  (define (x-ok? item ls cur-row target-row)
    (cond ((null? ls) true)
          ((in-x? item (car ls) (- target-row cur-row)) false)
          (else (x-ok? item (cdr ls) (+ cur-row 1) target-row))))
  (let ((q1 (amb 1 2 3 4 5 6 7 8))
        (q2 (amb 1 2 3 4 5 6 7 8)))
    (require (distinct? (list q1 q2)))
    (require (x-ok? q2 (list q1) 1 2))
    (let ((q3 (amb 1 2 3 4 5 6 7 8)))
      (require (distinct? (list q1 q2 q3)))
      (require (x-ok? q3 (list q1 q2) 1 3))
      (let ((q4 (amb 1 2 3 4 5 6 7 8)))
        (require (distinct? (list q1 q2 q3 q4)))
        (require (x-ok? q4 (list q1 q2 q3) 1 4))
        (let ((q5 (amb 1 2 3 4 5 6 7 8)))
          (require (distinct? (list q1 q2 q3 q4 q5)))
          (require (x-ok? q5 (list q1 q2 q3 q4) 1 5))
          (let ((q6 (amb 1 2 3 4 5 6 7 8)))
            (require (distinct? (list q1 q2 q3 q4 q5 q6)))
            (require (x-ok? q6 (list q1 q2 q3 q4 q5) 1 6))
            (let ((q7 (amb 1 2 3 4 5 6 7 8)))
              (require (distinct? (list q1 q2 q3 q4 q5 q6 q7)))
              (require (x-ok? q7 (list q1 q2 q3 q4 q5 q6) 1 7))
              (let ((q8 (amb 1 2 3 4 5 6 7 8)))
                (require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))
                (require (x-ok? q8 (list q1 q2 q3 q4 q5 q6 q7) 1 8))
                (list q1 q2 q3 q4 q5 q6 q7 q8)))))))))

(solve-8-queen)
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
try-again
