;; Yes, if we give more restrictive require condition at the top, then we won't waste computing resouce
;; since it's if a combination already pass restrictive condition, then it should left fewer combination
;; for rest require testing.
(load "ch4-ambeval.scm")
