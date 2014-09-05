;;;  More procedures as arguments:

;;;  product(f, a, next, b) yielding the product of terms from a to be
;;;  with next as a function for determining the x-values of the function

(define (product f a next b)
  (if (> a b) 1
      (* (f a) (product f (next a) next b))))


;;;
;;; factorial of n in terms of the product(term, a, next, b) function f
;;;

(define (factorialp n)
  (product identity 1 inc n))


;;;
;;;  pi-approx(n) to approximate pi using:
;;;  pi/4  =  2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 . . . (n terms)
;;;  RHS is equal to product of two products :
;;;  pd  =  2/3 * 4/5 * 6/7 * 8/9 . . . .
;;;  pn  =  4/3 * 6/5 * 8/7 * 10/9 . . . .
;;;  problem does not specify number of terms so will 
;;;  take n terms of pd and also of pn
;;;  build up the numerator and the denominator then
;;;  divide num / denom
;;;
;;;  pdn  =  2 * 4 * 6 * 8 * 10 . . .
;;;  pnn  =  4 * 6 * 8 * 10 . . . 
;;;
;;;  pdd  =  3 * 5 * 7 * 9 . . .
;;;  pnd  =  same

;;;  pdn = (product identity 2 next n)
;;;  pnn = (product identity 4 next n)
;;;  
;;;  pdd = (product identity 3 next n)
;;;  pnd = (product identity 3 next n)
;;;
;;;  all with next = (+ x 2)
;;;
 
(define (pi-approx n)
  (define (next x)
    (+ x 2))
  (* 4.0 (/ (* (product identity 2 next n) (product identity 4 next n))
	    (* (product identity 3 next (+ n 1)) (product identity 3 next n)))))

;;;  product through an iterative process :

(define (product-iter f a next b)
  (define (product-helper prod f a next b)
    (if (> a b)
	prod
	(product-helper (* prod (f a)) f (next a) next b)))
  (product-helper 1 f a next b))

(define (pi-approx2 n)
  (define (next x)
    (+ x 2))
  (* 4.0 (/ (* (product-iter identity 2 next n) (product-iter identity 4 next n))
	    (* (product-iter identity 3 next (+ n 1)) (product-iter identity 3 next n)))))
