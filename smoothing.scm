;;;  Ex. 1.44 p. 78
;;;  The idea of smoothing a function is an important
;;;  concept in signal processing.
;;;  If f is a function and dx is a small number, then
;;;  the smoothed version of f is the function whose value at x
;;;  is the average of f(x - dx), f(x), and f(x + dx).
;;;  Write a procedure smooth that takes as input
;;;  a procedure that computes f and returns a procedure that
;;;  computes the smoothed f.
;;;  It is sometimes valuable to smooth a function repeatedly
;;;  to obtain the n-smoothed function.
;;;  Show how to generate the n-fold smoothed function of a
;;;  function f using smoothed and repeated.

(define (average3 x y z)
  (/ (+ x y z) 3.0))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (average3 (f (- x dx)) (f x) (f (+ x dx)))))

((smooth square) 5)

((smooth abs) 0)

;;; come back to make plots with the smooth function and
;;; with repeated on smooth


(define (repeat-smooth f n)
  (repeated (smooth f) n))


;;;  Ex. 1.45 p. 78
;;;  We saw in section 1.3.3 that attempting to
;;;  compute square roots by finding a fixed point
;;;  of y --> x/y does not converge but that this 
;;;  can be fixed by average damping. The same
;;;  method works for finding cube roots as fixed
;;;  points of the average-damped y --> x/y^2.
;;;  Unfortunately, the process does not work for 
;;;  4th roots. A single average damp is not enough to 
;;;  make a fixed-point search for y --> x/y^3 converge.
;;;  On the other hand, if we average damp twice
;;;  (i.e., average damp of average damp of y --> x/y^3)
;;;  the fixed-point search does converge.
;;;  Experiment to determine how many average damps are required
;;;  to compute nth roots as a fixed-point search based on 
;;;  repeated average damping of y --> x/y^(n-1).
;;;  Use this to implement a simple procedure for 
;;;  computing nth roots using fixed-point, average-damp,
;;;  and then the repeated procedure of exercise 1.43.
;;;  Assume that arithmetic operations are available as 
;;;  primitives.

(define (average a b)
  (/ (+ a b) 2.0))

(define (eighth-root x)
  (fixed-point (average-damp (average-damp (average-damp (lambda (y) (/ x (* y y y y y y y))))))
	       1.0))


;;; from experiement
;;; the number of average damps for root-n is:
;;; (floor (/ (log n) (log 2)))



(eighth-root 65536)



;;;  the function power returns
;;;  x^n

(define (power x n)
  (cond ((= n 0) 1)
	((= n 1) x)
	(else (* x (power x (- n 1))))))

;;; test:

(power 2 5)

(power 4 3)

(power 35 0)

(power 12 2)



;;; root returns the nth root of number x:

(define (root x n) 
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2)))) (lambda (y) (/ x (power y (- n 1)))))
	       1.0))

(root 144 2)

(root 78125 7)
