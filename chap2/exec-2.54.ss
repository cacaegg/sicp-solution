(define (equal? lsa lsb)
 (cond ((and (null? lsa) (null? lsb)) #t)
       ((not (eq? (car lsa) (car lsb))) #f)
       (else (equal? (cdr lsa) (cdr lsb)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
