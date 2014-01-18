(((lambda () 
    (lambda (x) (+ x 1))))
 10)

; If we don't force operator, then the internal lambda will not
; be evaluated before apply
