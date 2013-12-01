(define (make-dequeue)
 (let ((front '())
       (rear '()))
  (define (make-item val pre next) (list val pre next))
  (define get-val car)
  (define previous-item cadr)
  (define next-item caddr)
  (define (set-previous-item! item new-pre)
   (set-car! (cdr item) new-pre))
  (define (set-next-item! item new-next)
   (set-car! (cddr item) new-next))

  (define (set-front-ptr! item) (set! front item))
  (define (set-rear-ptr! item) (set! rear item))
  (define (empty-queue?) (null? front))
  (define (front-queue)
   (if (empty-queue?)
       (error 'front-queue "ERROR called with an empty queue" queue)
       (get-val front)))
  (define (rear-queue)
   (if (empty-queue?)
       (error 'rear-queue "ERROR called with empty queue" queue)
       (get-val rear)))
  (define (front-insert! val)
   (let ((new-item (make-item val '() '())))
    (cond ((empty-queue?)
           (set-front-ptr! new-item)
           (set-rear-ptr! new-item))
          (else
           (set-next-item! new-item front)
           (set-previous-item! front new-item)
           (set-front-ptr! new-item)))))
  (define (rear-insert! val)
   (let ((new-item (make-item val '() '())))
    (cond ((empty-queue?)
           (set-front-ptr! new-item)
           (set-rear-ptr! new-item))
          (else
           (set-next-item! rear new-item)
           (set-previous-item! new-item rear)
           (set-rear-ptr! new-item)))))
  (define (front-delete!)
   (cond ((empty-queue?)
          (error 'front-delete! "ERROR called with an empty queue"))
         (else
          (set-front-ptr! (next-item front))
          (set-previous-item! front '()))))
  (define (rear-delete!)
   (cond ((empty-queue?)
          (error 'rear-delete! "ERROR called with an empty queue"))
         (else
          (set-rear-ptr! (previous-item rear))
          (set-next-item! rear '()))))
  (define (print-queue)
   (define (traverse cur-item)
    (if (null? (next-item cur-item))
        (cons (get-val cur-item) '())
        (cons (get-val cur-item)
              (traverse (next-item cur-item)))))
   (if  (empty-queue?) 
        '()
        (traverse front)))
  (define (dispatch m)
   (cond ((eq? m 'front) front-queue)
         ((eq? m 'rear) rear-queue)
         ((eq? m 'front-insert!) front-insert!)
         ((eq? m 'rear-insert!) rear-insert!)
         ((eq? m 'front-delete!) front-delete!)
         ((eq? m 'rear-delete!) rear-delete!)
         ((eq? m 'print) 
          (lambda () (display (print-queue)) (newline)))
         ((eq? m 'empty?) empty-queue?)
         (else 
          (error 'dispatch "ERROR no such operation in queue" m))))
  dispatch))

(define (empty-queue? q) ((q 'empty?)))
(define (front-insert-queue! q val) 
 ((q 'front-insert!) val))
(define (rear-insert-queue! q val)
 ((q 'rear-insert!) val))
(define (front-delete-queue! q) ((q 'front-delete!)))
(define (rear-delete-queue! q) ((q 'rear-delete!)))
(define (print-queue q) ((q 'print)))

(define q1 (make-dequeue)) 
(print-queue q1)
(front-insert-queue! q1 'a)
(front-insert-queue! q1 'b)
(rear-insert-queue! q1 'c)
(print-queue q1)
(front-delete-queue! q1)
(print-queue q1)
(rear-delete-queue! q1)
(print-queue q1)