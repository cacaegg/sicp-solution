(define (outline-segments)
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.0 1.0))
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 1.0 0.0))
  (make-segment
   (make-vect 0.0 1.0)
   (make-vect 1.0 1.0))
  (make-segment 
   (make-vect 1.0 1.0)
   (make-cect 1.0 0.0))))
(define (output-painter)
 (segment->painter outline-segments))

(define (x-segments)
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0))
  (make-segment
   (make-vect 1.0 0.0)
   (make-vect 0.0 1.0))))
(define (x-painter)
 (segment->painter x-segments))

(define (diamond-segments)
 (list
  (make-segment
   (make-vect 0.5 0.0)
   (make-vect 1.0 0.5))
  (make-segment
   (make-vect 1.0 0.5)
   (make-vect 0.5 1.0))
  (make-segment
   (make-vect 0.5 1.0)
   (make-vect 0.0 0.5))
  (make-segment
   (make-vect 0.0 0.5)
   (make-vect 0.5 0.0))))
(define (diamond-painter)
 (segment->painter diamond-segments))
