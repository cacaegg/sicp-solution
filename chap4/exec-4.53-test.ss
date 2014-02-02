(define (require p)
  (if (not p) (amb)))

;; map procedure
(define map 
  (lambda (proc ls)
    (if (null? ls)
        '()
        (cons (proc (car ls))
              (map proc (cdr ls))))))

;; append procedure
(define append 
  (lambda (ls1 ls2)
    (if (null? ls1)
        ls2
        (cons (car ls1)
              (append (cdr ls1) ls2)))))

(define (fast-prime? n times)
 (cond ((= times 0) #t)
       ((fermat-test n) (fast-prime? n (- times 1)))
       (else #f)))

(define (fermat-test n)
 (define (try-it a)
  (= (expmod a n n) a))
 (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
 (cond ((= exp 0) 1)
       ((even? exp) 
        (remainder (* (expmod base (/ exp 2) m)
                      (expmod base (/ exp 2) m))
                    m))
       (else
        (remainder (* base (expmod base (- exp 1) m))
                   m))))

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

(define (prime-sum? pair)
 (fast-prime? (+ (car pair) (car (cdr pair))) 10))

(define (enum-interval low high)
 (cond ((> low high) '())
       (else (cons low (enum-interval (+ low 1) high)))))

(define (unique-pair n)
 (flatmap (lambda (i) 
           (map (lambda (j) (list j i))
                (enum-interval 1 (- i 1))))
          (enum-interval 1 n)))

(define (prime-sum-pairs n)
 (filter prime-sum?
         (unique-pair n)))

(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pairs 10)))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))
