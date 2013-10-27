(define (flip-horiz painter)
 (transform-painter painter
                    (make-vect 1.0 0.0)
                    (make-vect 0.0 0.0)
                    (make-vect 1.0 1.0)))

(define (rot180 painter)
 (tranform-painter painter
                   (make-vect 1.0 1.0)
                   (make-vect 0.0 1.0)
                   (make-vect 1.0 0.0)))

(define (rot270 painter)
 (transform-painter painter
                    (make-vect 1.0 0.0)
                    (make-vect 1.0 1.0)
                    (make-vect 0.0 0.0)))
