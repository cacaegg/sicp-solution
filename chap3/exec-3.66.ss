; (5, 5) is sum([2**4, 2**3, 2**2, 2**1])
;
; Given (n, m) whene n <= m, then 
;
; NPairs(n,m) = NPairs(n, n - 1) + NPairs(n, i)
; where n <= i <= m 
;
; Then NPairs(n, n - 1) = sum([2**(n-1), 2**(n-2) ..., 2**0])
; And NPairs(n, i) = 2**(n-1) + (i - n) * 2**n
