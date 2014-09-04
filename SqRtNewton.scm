;;; Compute the square root of x using Newton's method
;;;
;;; This method tests whether a guess is good enough
;;;
;;; A guess is good enough if the square of the guess 
;;; is within a specified neighborhood of x
;;;
;;; guess is returned if it is good enough
;;; Otherwise procedure tries a new guess equal to
;;; the average of x and x/guess

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (/ (abs (- guess (/ x guess))) guess) 0.01))

(define (sqrt x)
  (sqrt-iter 1.0 x))
