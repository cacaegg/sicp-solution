(define (make-vect x y)
 (list x y))

(define (xcor-vect v)
 (car v))

(define (ycor-vect v)
 (cadr v))

(define (add-vect v1 v2)
 (make-vect
  (+ (xcor-vect v1) (xcor-vect v2))
  (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
 (make-vect
  (- (xcor-vect v1) (xcor-vect v2))
  (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
 (make-vect
  (* s (xcor-vect v))
  (* s (ycor-vect v))))

(let ((v1 (make-vect 2 3))
       (v2 (make-vect 1 2)))
  (add-vect v1 v2))

(let ((v1 (make-vect 2 3))
       (v2 (make-vect 1 2)))
  (sub-vect v1 v2))


(let ((v1 (make-vect 2 3)))
  (scale-vect v1 4))
