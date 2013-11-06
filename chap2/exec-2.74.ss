; a.
(define (install-cacaegg-division)
 ;; local structures
 (define (create-record name) 
  (list 'cacaegg name))
 (define (get-record records name)
  (cond ((null? records) '())
        ((same-person? (car records) name)
         (car records))
        (else (get-record (cdr records) name))))
 (define (id-record record) (cadr record))
 ;; interfaces
 (put 'cacaegg 'get-record get-record)
 (put 'cacaegg 'get-idenifier id-record))

(define (get-division record) (car record))
(define (make-file division file) (cons division file))
(define (get-division file) (car file))
(define (get-file file) (cdr file))

; a. Usage at headquarter
(install-cacaegg-division)
(define (get-record employee file)
 ((get (get-division file) 'get-record) employee))
; b. Headquarter's get-salary
(define (get-salary employee file)
 ((get (get-division file) 'get-salary) employee))
; c. Headquater's find-employee-record
(define (find-employee-record employee files)
 (if (null? files) 
     #f
     (let ((emp ((get (get-division (car file)) 
                      'get-record) 
                 employee)))
      (if (= #f emp)
          (find-employee-record employee (cdr files))
          emp))))
; d. Just ask new company provides corresponding package as cacaegg
;    and install it.
