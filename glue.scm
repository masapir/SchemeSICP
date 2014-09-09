;;;  Sec. 2.1.3 p. 90
;;;
;;;  Just a single constraint on the constructor
;;;  and selectors for rational numbers:
;;;  (constructor n d) constructs a rational r
;;;  and (numerator r) = n, (denominator r) = d iff
;;;  n/d  =  (numerator r)/(denominator r)

;;;  We can build cons, car, and cdr using procedures :

(define (glue x y)
  (define (dispatch code)
    (cond ((= code 0) x)
	  ((= code 1) y)
	  (else (error "dispatch code is not 0 or 1 GLUE" code))))
  dispatch)

(define (first-component pair)
  (pair 0))

(define (second-component pair)
  (pair 1))

(define p1 (glue 10 20))

(first-component p1)

(second-component p1)
