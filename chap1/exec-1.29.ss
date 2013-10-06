(define (sum term a next b)
 (if (> a b)
     0
     (+ (term a)
        (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (integral f a b n)
 (define h (/ (- b a) n))
 (define (simpson-term k)
  (define y (f (+ a (* k h))))
  (cond ((or (= k 0) (= k n)) y)
        ((even? k) (* 4 y))
        (else (* 2 y))))
 (* (/ h 3) (sum simpson-term a inc b)))

(integral cube 0 1 100)
(integral cube 0 1 1000)
