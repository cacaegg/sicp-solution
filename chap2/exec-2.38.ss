(define (fold-left op initial sequence)
 (define (iter result rest)
  (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
 (iter initial sequence))

(define (fold-right op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (fold-right op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
; > (/ 1 (/ 2 (/ 3 1)))
(fold-left / 1 (list 1 2 3))
; > (iter / (/ 1 1) (list 2 3))
; > (iter / (/ 1 2) (list 3))
; > (iter / (/ (/ 1 2) 3) '())

(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

; op should keep the same result of (op a b) & (op b a)
