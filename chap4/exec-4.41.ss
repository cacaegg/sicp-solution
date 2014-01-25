(define baker (map (lambda (x) (list 'baker x))
                   (filter (lambda (x) (not (= x 5))) 
                           (list 1 2 3 4 5))))
(define cooper (map (lambda (x) (list 'cooper x))
                    (filter (lambda (x) (not (= x 1)))
                            (list 1 2 3 4 5))))
(define fletcher (map (lambda (x) (list 'fletcher x)) 
                      (filter (lambda (x) (and (not (= x 1)) (not (= x 5))))
                              (list 1 2 3 4 5))))
(define miller (map (lambda (x) (list 'miller x))
                    (list 1 2 3 4 5)))
(define smith (map (lambda (x) (list 'smith x))
                   (list 1 2 3 4 5)))

(define people-name car)
(define people-floor cdr)

(define (lookup-by-name name ls)
  (cond ((null? ls) '())
        ((eq? name (car ls))
         (cons (car ls) (cadr ls)))
        (else
         (lookup-by-name name (cdr ls)))))

(define (flat-ls ls)
  (if (null? ls)
      '()
      (append (car ls) 
              (flat-ls (cdr ls)))))

(define (item-on-list item ls)
  (map (lambda (x) (append item x)) ls))

(define (flatmap proc ls)
  (flat-ls (map proc ls)))

(define possibilities 
  (flatmap
    (lambda (item) (item-on-list item smith))
    (flatmap
      (lambda (item) (item-on-list item miller))
      (flatmap 
        (lambda (item) (item-on-list item fletcher))
        (flatmap (lambda (item) (item-on-list item cooper))
                 baker)))))

(define conditions
  (list (lambda (ls)
          (let ((miller (lookup-by-name 'miller ls))
                (cooper (lookup-by-name 'cooper ls)))
            (> (people-floor miller) (people-floor cooper))))
        (lambda (ls)
          (let ((smith (lookup-by-name 'smith ls))
                (fletcher (lookup-by-name 'fletcher ls)))
            (not (= (abs (- (people-floor smith) 
                            (people-floor fletcher))) 
                    1))))
        (lambda (ls)
          (let ((cooper (lookup-by-name 'cooper ls))
                (fletcher (lookup-by-name 'fletcher ls)))
            (not (= (abs (- (people-floor cooper) 
                            (people-floor fletcher))) 
                    1))))
        (lambda (ls)
          (let* ((floors (list 1 2 3 4 5))
                 (floor-cnts  (map (lambda (f) 
                                     (length (filter 
                                               (lambda (ls-f) (eq? f ls-f)) 
                                               ls)))
                                   floors)))
            (= (length (filter (lambda (f-cnt) (= f-cnt 1)) floor-cnts)) 5)))))

(define (apply-conditions results conditions)
  (if (null? conditions) 
      results
      (apply-conditions (filter (car conditions) results)
                        (cdr conditions))))

(display (apply-conditions possibilities conditions))(newline)
