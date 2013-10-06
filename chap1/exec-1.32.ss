(define (accumulate-rec combiner null-value term a next b)
 (if (> a b)
     null-value
     (combiner (term a) 
      (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
 (define (iter cur result)
  (if (> cur b)
      result
      (iter (next cur) (combiner (term cur) result))))
 (iter a null-value))

(define (identity x) x)

(define (sum-rec a b)
 (accumulate-rec + 0 identity a 1+ b))

(define (product-rec a b)
 (accumulate-rec * 1 identity a 1+ b))

(sum-rec 1 10)
(product-rec 1 10)

(define (sum-iter a b)
 (accumulate-iter + 0 identity a 1+ b))

(define (product-iter a b)
 (accumulate-iter * 1 identity a 1+ b))

(sum-iter 1 10)
(product-iter 1 10)
