(define (square x) (* x x))
(define (square-list items)
 (define (iter things answer)
  (if(null? things)
     answer
     (iter (cdr things)
           (cons (square (car things))
                 answer))))
 (iter items '()))

; For each iterate, the answer will be cons to second pointer
; of the pair. So the answer will be shown in reverse way.
(square-list (list 1 2 3 4 5))

(define (square-list items)
 (define (iter things answer)
  (if(null? things)
     answer
     (iter (cdr things)
           (cons answer
                 (square (car things))))))
 (iter items '()))
; This time, next pair will be point by first element in each pair
; So still won't be a normal list (next pair point by second element)
(square-list (list 1 2 3 4 5))

