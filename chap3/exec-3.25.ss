(define (make-table same-key?)
 (let ((local-table (list '*table*)))
  (define tblassoc
   (lambda (x ls)
    (cond ((null? ls) #f)
          ((same-key? (caar ls) x) (car ls))
          (else (tblassoc x (cdr ls))))))
  (define (lookup cur-table key-ls)
   (if (null? key-ls)
       #f
       (let ((record (tblassoc (car key-ls) (cdr cur-table))))
        (cond ((eq? #f record) #f)
              ((null? (cdr key-ls)) (cdr record))
              (else
               (lookup record (cdr key-ls)))))))
  (define (insert! cur-table value key-ls)
   (if (null? key-ls)
       'nok
       (let ((record (tblassoc (car key-ls) (cdr cur-table))))
        (cond ((eq? #f record)
               (if (null? (cdr key-ls))
                   (set-cdr! cur-table
                         (cons (cons (car key-ls) value) (cdr cur-table)))
                   (let ((new-table (list (car key-ls))))
                    (set-cdr! cur-table
                              (cons new-table (cdr cur-table)))
                    (insert! new-table value (cdr key-ls))))
               'ok)
              ((null? (cdr key-ls))
               (set-cdr! record value)
               'ok)
              (else
               (insert! record value (cdr key-ls)))))))
  (define (dispatch m)
   (cond ((eq? m 'lookup-proc) 
          (lambda (ls) (lookup local-table ls)))
         ((eq? m 'insert-proc!) 
          (lambda (ls val) (insert! local-table val ls)))
         (else (error 'dispatch "Unknown operation -- TABLE" m))))
  dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(put (list 'C) 4)
(put (list 'A 'a 'c) 1)
(put (list 'A 'b) 3)
(put (list 'B 'b) 2)
(get (list 'A 'a 'c))
(get (list 'A 'b))
(get (list 'B 'b))
(get (list 'C))
