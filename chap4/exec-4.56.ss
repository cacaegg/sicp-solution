(and (supervisor ?people (Bitdiddle Ben))
          (address ?people ?))

(and (salary (Bitdiddle Ben) ?ben-salary)
     (salary ?person ?amount)
     (lisp-value <= ?amount ?ben-salary))

(and (supervisor ?people ?s-people)
     (job ?s-people ?)
     (not (job ?s-people (computer . ?department))))
