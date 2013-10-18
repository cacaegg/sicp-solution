(define (same-parity pivot . ls)
 (define (make-check)
  (if (even? pivot)
       even?
       odd?))
 (define (processor remain result same?)
  (if (null? remain)
      result
      (let ((item (car remain)))
       (if (same? item)
           (processor (cdr remain)
                      (append result (list item))
                      same?)
           (processor (cdr remain)
                      result
                      same?)))))
 (processor ls (list pivot) (make-check)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
