;;; Due to if one of the stream is an infinite stream, then delay will able to
;;; fecth answer from another stream.
(assert! (name mark))
(assert! (age makr 72))
(assert! (name lucy))
(assert! (age lucy 43))
(assert! (name tom))
(assert! (age tom 20))
(assert! (rule (older ?less ?more)
	  (and (age ?less ?less-age)
	   (age ?more ?more-age)
	   (lisp-value < ?less-age ?more-age))))
(assert! (rule (name ?w) (name ?w)))

(or (older ?a ?b) (name ?w))
