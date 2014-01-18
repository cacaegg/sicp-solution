; For any recursive program, non-memorized version will run slower than memoried version.
; Due to it need to analyzed expression for each recursion.
;
; Such as fib number
;

(define (square x)
  (* x x))

(square (id 10))
; 100

count
; 1 (memorized)
; 2 (non-memorized)
