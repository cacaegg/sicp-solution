(define (make-monitored f)
 (define count 0)
 (define (inc-count) (set! count (1+ count)))
 (define (call-num) count)
 (define (reset-count) (set! count 0))
 (define (dispatch msg)
  (cond ((eq? msg 'how-many-calls?) call-num)
        ((eq? msg 'reset-count) reset-count)
        (else (begin (inc-count) (f msg)))))
 dispatch)

(define s (make-monitored sqrt))

(s 2)
((s 'how-many-calls?))
(s 4)
(s 6)
(s 10)
((s 'how-many-calls?))
((s 'reset-count))
((s 'how-many-calls?))
(s 19)
((s 'how-many-calls?))
