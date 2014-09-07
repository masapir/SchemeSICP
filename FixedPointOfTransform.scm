;;;  Derivative:  
;;;  Dg(x)  =  [g(x + dx) - g(x)] / dx

(define dx 0.00001)

(define (derivative g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


;;; cube function:

(define (cube x) (* x x x))

((derivative cube) 5)


;;;  Now with the derivative function we can 
;;;  build the Newton transformation:

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derivative g) x)))))


;;;  With the Newton transform we can build Newton's 
;;;  method to find roots of g(x):

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

;;;  The square root function can be expressed
;;;  as the function yielded when you search
;;;  for the zeros of the function y maps
;;;  to y^2 - x
;;;  y^2 - x = 0  <=>
;;;  y^2 = x      <=>
;;;  y  =  sqrt(x).
;;;  y = y^2 - x : zeros can be found using Newton's method:

(define (sqrt x)
  (newton-method (lambda (y) (- (* y y) x))
		 1.0))


;;;  General function to find fixed-point of a transformation of a function:

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;; re-casting square root with this generalized function:

(define (sqrt-alt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

;;; re-casting square root by Newton with this generalized function:

(define (sqrt-alt2 x)
  (fixed-point-of-transform (lambda (y) (- (* y y) x))
			    newton-transform
			    1.0))


;;;  Ex. 1.40 p. 77
;;;  Define a procedure cubic that can be used with newton-method as in
;;;  (newton method (cubic a b c) 1) to approximate zeros of the cubic
;;;  x^3 + a*x^2 + b*x + c.

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a (* x x)) (* b x) c)))

(newton-method (cubic 1 2 3) 1)

(newton-method (cubic 1 1 1) 1)
