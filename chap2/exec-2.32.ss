(define (subsets s)
 (if (null? s)
     (list '())
     (let ((rest (subsets (cdr s))))
      (append rest (map 
                    (lambda (x) (cons (car s) x))
                    rest)))))

(subsets (list 1 2 3))

; For each pair, use car and map each item in subset of cdr.
; This can make combination of car with each cdr item.
; e.g. (1 2) 
; (car (list 1 2)) 
; > 1
; 
; (subsets (cdr (list 1 2))) 
; > (subsets (2))
; map cons car 1 with each item in (subset (2))
;
; And (subset (2)), with (cdr (2)) is null. Hence, only generate
; ((), 2)
;
; Return to map 1 to (() (2)). (cons 1 x) will generate ((1) (1 2))
;
; Finally, append two subset (append (() (2)) ((1) (1 2))) 
; > (() (2) (1) (1 2))
(subsets (list 1 2))
