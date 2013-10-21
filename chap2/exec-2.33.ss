(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (map p sequence)
 (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(map 1+ (list 1 2 3 4 5))

(define (append seq1 seq2)
 (accumulate cons seq2 seq1))

(append (list 4 5 6) (list 1 2 3))

(define (length sequence)
 (accumulate (lambda (now result) (+ 1 result)) 0 sequence))

(length (list 1 2 3 4 5))
