(define (product-rec term a next b)
 (if (> a b)
     1
     (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
 (define (iter a sum)
  (if (> a b)
      sum
      (iter (next a) (* (term a) sum))))
 (iter a 1))

(define (identity x) x)    

(product-rec identity 1 1+ 10)
(product-iter identity 1 1+ 10)

(define (pi-apoxi-rec k)
 (define (pi-term n)
  (define (pi-numerator q)
   (cond ((= n 1) 2)
         (else (+ 2 (* 2 q)))))
  (define (pi-denominator ceil)
   (+ (* 2 ceil) 1))
  (/ (pi-numerator (quotient n 2)) (pi-denominator (ceiling (/ n 2)))))
 (* 4.0 (product-rec pi-term 1 1+ k)))


(define (pi-apoxi-iter k)
 (define (pi-term n)
  (define (pi-numerator q)
   (cond ((= n 1) 2)
         (else (+ 2 (* 2 q)))))
  (define (pi-denominator ceil)
   (+ (* 2 ceil) 1))
  (/ (pi-numerator (quotient n 2)) (pi-denominator (ceiling (/ n 2)))))
 (* 4.0 (product-iter pi-term 1 1+ k)))

(pi-apoxi-rec 512)
(pi-apoxi-iter 512)
