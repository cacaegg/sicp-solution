(define (make-table same-key?)
 (let ((local-table (list '*table*)))
  (define tblassoc
   (lambda (x ls)
    (cond ((null? ls) #f)
          ((same-key? (caar ls) x) (car ls))
          (else (tblassoc x (cdr ls))))))
  (define (lookup key-1 key-2)
   (let ((subtable (tblassoc key-1 (cdr local-table))))
    (if subtable
        (let ((record (tblassoc key-2 (cdr subtable))))
         (if record
             (cdr record)
             #f))
        #f)))
  (define (insert! key-1 key-2 value)
   (let ((subtable (tblassoc key-1 (cdr local-table))))
    (if subtable
        (let ((record (tblassoc key-2 (cdr subtable))))
         (if record
             (set-cdr! record value)
             (set-cdr! subtable
                       (cons (cons key-2 value)
                             (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr local-table))))
    'ok))
  (define (dispatch m)
   (cond ((eq? m 'lookup-proc) lookup)
         ((eq? m 'insert-proc!) insert!)
         (else (error 'dispatch "Unknown operation -- TABLE" m))))
  dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'A 'a 1)
(get 'A 'a)
(put 1 2 'a)
(get 1 2)
