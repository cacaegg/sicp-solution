(car ''abracadabra)
; Since ' is a quote precedure,
; ''abracadabra will be translate to (quote (quote abracadabra))
; which generate 'abracadabra
; so its car returns quote
