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

(define (all-ranking-combinations)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (xor-require (= kitty 2) (= betty 3))
    (xor-require (= ethel 1) (= joan 2))
    (xor-require (= joan 3) (= ethel 5))
    (xor-require (= kitty 2) (= mary 4))
    (xor-require (= mary 4) (= betty 1))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list 'betty betty 
          'ethel ethel
          'joan joan
          'kitty kitty
          'mary mary)))

(all-ranking-combinations)
try-again

