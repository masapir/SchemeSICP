;;;  Ex. 1.46 p. 78
;;;  Write a procedure iterative-improve that takes two procedures
;;;  as arguments:
;;;  a method for telling whether a guess is good enough and
;;;  a method for improving a guess.
;;;  iterative-improve should return a procedure that takes a guess as
;;;  argument and keeps improving the guess until it is good enough.
;;;  Re-write sqrt and fixed-point using iterative-improve.


(define (iterative-improve okay? improve)
  (define (iterator guess)
    (if (okay? guess)
	guess
	(iterator (improve guess))))
  (lambda (guess)
    (iterator guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.000001))
  (define (average a b)
    (/ (+ a b) 2.0))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 25)

(sqrt 144)


;;; now for fixed-point with iterative-improve :


(define (fixed-point2 f guess)
  (define tolerance 0.00001)
  (define (tolerance? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve tolerance? improve) guess))

(fixed-point2 cos 1.0)
