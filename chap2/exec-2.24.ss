(list 1 (list 2 (list 3 4)))

; Box-and-Pointer structure
; OO-->OO-->OO-->OX
; |    |    |    |
; v    v    v    v
; 1    2    3    4
;
; Tree representation
;   (1 (2 (3 4)))
;   /           \
;  v             v
; 1          (2 (3 4))
;           /         \
;          v           v
;         2           (3 4)
;                    /     \
;                   v       v
;                  3         4
