;;;
;;;  Fixed points of functions
;;;

;;;
;;;  A fixed point of f(x) is a point t such that f(t) = t
;;;  For example 1 is a fixed point of f(x) = x^2.  1^2 = 1.
;;;  0 is also a fixed point of f(x) = x^2.  

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? y1 y2)
    (< (abs (- y2 y1)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;;
;;; for square root :  y^2 = x
;;; y = x/y
;;; y, the square root of x, is the fixed point of the function
;;; f(y) = x/y :

(define (fp-sqrt x)
  (fixed-point (lambda (y) (/ x y))
	       1.0))

;;; the function above goes into an infinite loop as its process
;;; oscillates between a y1 and y2 on each side of y, the square root of x

;;; damp the guess :

(define (fpd-sqrt x)
  (fixed-point (lambda (y) (average (/ x y) y))
	       1.0))


;;; Golden ratio as a fixed point
;;; Phi = (1 + 5^1/2) / 2 =~ 1.6180
;;; x^2 = x + 1
;;; divide both sides by x:  x = 1 + 1/x
;;; so if you consider the function f(x) = 1 + 1/x
;;; the fixed point of f is x = 1 + 1/x
;;; this is also the solution to x^2 = x + 1
;;; i.e., x is the golden ratio

(define (golden)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))


;;; more generally, transform an equation into the form
;;; x = f(x).  In this form, the fixed point of f(x)
;;; solves the original equation too.  You might be able 
;;; to compute the fixed point of f(x).
;;; 
;;; Graphically, this algebraic manipulation corresponds
;;; to reformulating the equation to isolate the identity function.
;;; In this, we can determine whether the first equation is equivalent to an equation 
;;; relating the identity function to another function.


;;;  SICP ex. 1.36 p. 70
;;;  Modified fixed-point to display guesses:

(define (fixed-pointm f first-guess)
  (define (close-enough? y1 y2)
    (< (abs (- y2 y1)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display "next guess : ")
      (display next)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;;  Solve x^x = 1000 (eq. 1)
;;;  x^x =  e^ln(x^x)
;;;      =  e^x*lnx
;;;  so e^x*lnx  =  1000
;;;              = e^ln(1000)
;;;  so x*lnx  =  ln(1000)
;;;  now we obtain an equation equivalent 
;;;  to (eq. 1) but expressing the identity function
;;;  consequently we can solve by finding the fixed point:
;;;  we have : x  = ln(1000)/ln(x)

(define (solve-eq1)
  (fixed-pointm (lambda (x) (/ (log 1000) (log x)))
		4.0))

;;;  Let's try that for x^x = 10000
;;;  we have x = ln(10000)/ln(x)  (call this (eq. 2))

(define (solve-eq2)
  (fixed-pointm (lambda (x) (/ (log 10000) (log x)))
		5.0))
