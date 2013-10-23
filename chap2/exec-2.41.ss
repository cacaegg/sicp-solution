(define (filter predicate sequence)
 (cond ((null? sequence) '())
       ((predicate (car sequence))
        (cons (car sequence)
              (filter predicate (cdr sequence))))
       (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
 (accumulate append '() (map proc seq)))

(define (enum-interval low high)
 (cond ((> low high) '())
       (else (cons low (enum-interval (1+ low) high)))))

(define (unique-pair n)
 (flatmap (lambda (i) 
           (flatmap (lambda (j) 
                     (map (lambda (k) (list i j k))
                          (enum-interval 1 (1- j))))
                    (enum-interval 2 (1- i))))
          (enum-interval 3 n)))

(unique-pair 4)

(define (trip-sum? p s)
 (= (+ (car p) (cadr p) (caddr p)) s))

(define (sum-pairs n s)
 (filter (lambda (p) (trip-sum? p s))
         (unique-pair n)))

(sum-pairs 10 18)
