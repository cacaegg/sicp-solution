(define (add-rat x y)
 (make-rat (+ (* (numer x) (denom y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(define (sub-rat x y)
 (make-rat (- (* (numer x) (numer y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(define (mul-rat x y)
 (make-rat (* (numer x) (numer y))
           (* (denom x) (denum y))))

(define (div-rat x y)
 (make-rat (* (numer x) (denom y))
           (* (numer y) (denom x))))

(define (equal-rat? x y)
 (= (* (numer x) (denom y))
    (* (numer y) (denom x))))

(define (make-rat n d)
 (let ((g (gcd n d)))
  (cond ((< d 0) 
         (let ((n (* -1 n)) (d (abs d)))
          (cons (/ n g) (/ d g))))
        (else
          (cons (/ n g) (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(make-rat 3 -6)
(make-rat -2 4)
(make-rat -3 -9)
