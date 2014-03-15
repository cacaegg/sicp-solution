;; free = p1
(define x (cons 1 2))
;; > (assign (reg x) (op cons) (const 1) (const 2))
;; > (perform (op vector-set!) (reg the-cars) (reg free) (const 1))
;; > (perform (op vector-set!) (reg the-cdrs) (reg free) (const 2))
;; > (assign (reg x) (reg free))
;; > (assign (reg free) (op +) (reg free) (const 1))
;; x = p1
;; free = p2
;;       index  0   1    2   ...
;; the-cars = |   | n1 |   | ...
;; the-cdrs = |   | n2 |   | ...
(define y (list x x))
;; > (assign (reg y) (op cons) (reg x) (perform (op cons) (reg x) (const empty)))
;; x = p1
;; free = p3
;;       index  0   1    2   ...
;; the-cars = |   | n1 | p1 | ...
;; the-cdrs = |   | n2 | e0 | ...
;; > (assign (reg y) (op cons) (reg x) p2)
;; x = p1
;; y = p3
;; free = p4
;;       index  0   1    2    3    4   ...
;; the-cars = |   | n1 | p1 | p1 |   | ...
;; the-cdrs = |   | n2 | e0 | p2 |   | ...

;; _____
;; | x |      |---|---|
;; |___| ---->| 1 | 2 |
;;            |___|___|
;; _____        ^
;; | y |        |------------|
;; |   |        |            |
;; |___| ---->|-|-|---|    |-|-|---|
;;            | | | ------>| | | e0|
;;            |___|___|    |___|___|