;;;  Procedures as returned values
;;;  P. 72


;;;  An average damping function returns a function
;;;  that is the average of f(x) and x, i.e.,
;;;  damp(x)  =  (x + f(x)) / 2

(define (damping-f f)
  (lambda (x) (/ (+ (f x) x) 2.0)))

;;;  Even better, expressively by using the function average:

(define (average a b)
  (/ (+ a b) 2.0))

;;;  So now the damping function is:

(define (average-damp f)
  (lambda (x) (average (f x) x)))


;;;  The square root function computed using the fixed-point equation
;;;  can be re-written using average damp:

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

;;;  Re-use average-damp to compute the cube root of x using fixed point
;;;  y  =  x^3 so isolating the identity function:
;;;  y / x^2  =  x and we look for the fixed point of y / x^2

(define (square a)
  (* a a))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))
 