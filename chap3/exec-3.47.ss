(import (chezscheme))
(define (tail ls)
  (if (null? (cdr ls))
      ls
      (tail (cdr ls))))
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error 'make-account "Unknown request" m))))
    dispatch))
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))
(define (withdraw account amount)
  (let ((s (account 'serializer))
        (w (account 'withdraw)))
    ((s w) amount)))
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account 'serializer))
        (serializer2 (account 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'lock?)
             (car cell))
            ((eq? m 'acquire)
             (if (not (test-and-do! cell #t))
                 (the-mutex 'acquire)))
            ((eq? m 'aync-acquire)
             (test-and-do! cell #t))
            ((eq? m 'release) 
             (if (not (test-and-do! cell #f))
                 (the-mutex 'release)))
            ((eq? m 'aync-release)
             (test-and-do! cell #f))))
    the-mutex))
; a: in terms of mutex
(define (make-semaphore-m n)
  (define (make-keys cur)
    (if (>= cur n)
        (cons (make-mutex) '())
        (cons (make-mutex)
              (make-keys (1+ cur)))))
  (define (acquire-loop keys)
    (if (not ((car keys) 'aync-acquire))
        (acquire-loop (cdr keys))))
  (define (release-loop keys)
    (if (not ((car keys) 'aync-release))
        (release-loop (cdr keys))))
  (let ((keys (make-keys 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (acquire-loop keys))
            ((eq? m 'release)
             (release-loop keys))))
    (display (list "length of semaphore" (length keys)))(newline)
    (set-cdr! (tail keys) keys)
    the-semaphore))
; b: in terms of test-and-set!
(define (make-semaphore n)
  (define (make-keys cur)
    (if (>= cur n)
        (cons (list #f) '())
        (cons (list #f)
              (make-keys (1+ cur)))))
  (define (acquire-loop keys)
    (if (not (test-and-do! (car keys) #t))
        (acquire-loop (cdr keys))))
  (define (release-loop keys)
    (if (not (test-and-do! (car keys) #f))
        (release-loop (cdr keys))))
  (let ((keys (make-keys 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (acquire-loop keys))
            ((eq? m 'release)
             (release-loop keys))))
    (display (list "length of semaphore" (length keys) keys))(newline)
    (set-cdr! (tail keys) keys)
    the-semaphore))
(define (make-serializer)
  (let ((smp (make-semaphore 1)))
    (lambda (p)
      (define (serialized-p . args)
        (smp 'acquire)
        (let ((val (apply p args)))
          (smp 'release)
          val))
      serialized-p)))
(define (test-and-do! cell bool-val)
  (critical-section
    (if (eq? bool-val (car cell))
        #f
        (begin (set-car! cell bool-val)
               #t))))
(define toilet-keys (make-semaphore 2))
(define (make-people name)
 (lambda ()
   ;(display (list name "is goin' to toilet..."))(newline)
   (toilet-keys 'acquire)
   (display (list name "is 0rxing...."))(newline)
   (sleep (make-time 'time-duration 0 (1+ (random 7))))
   (toilet-keys 'release)))
   ;(display (list name "has left the toilet!"))(newline)))
(define peter (make-people 'peter))
(define john (make-people 'john))
(define mary (make-people 'mary))
(define jassie (make-people 'jassie))
(define kobe (make-people 'kobe))
(define paul (make-people 'paul))

(fork-thread mary)
(fork-thread jassie)
(fork-thread peter)
(fork-thread paul)
(fork-thread kobe)
(fork-thread john)

(sleep (make-time 'time-duration 0 15))