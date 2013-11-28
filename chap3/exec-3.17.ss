(define (count-pairs x)
 (define (in-list? x pair-list)
  (cond ((null? pair-list) #f)
        ((eq? x (car pair-list)) #t)
        (else (in-list? x (cdr pair-list)))))
 (define (count-iter x counted-pairs)
  (cond ((not (pair? x)) 0)
        ((in-list? x counted-pairs) 0)
        (else
         (+ (count-iter (car x) (cons x counted-pairs))
            (count-iter (cdr x) (cons x counted-pairs))
            1))))
 (count-iter x '()))

; (count-pairs x) never return
;   __________
;   |        |
;   v        |
; |---|---|  |
; | | | -----|
; |_|_|___|
;   |
;   v
;   A
(define x (cons 'A 'B))
(define y (cons 'C 'D))
(set-cdr! y x)
(set-cdr! x y)
(count-pairs x)
