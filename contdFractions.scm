;;;  Ex. 1.37 p. 71 on continued fractions
;;;  n, d are procedures of one argument, index,
;;;  n and d return the value of numerator_index and 
;;;  denominator_index for the continued fraction shown
;;;  on page 71.
;;;  Here is a procedure taking 3 arguments, n, d, k that
;;;  returns the continued fraction up to k terms.

;;;  First with a recursive process:

(define (cont-frac n d k)
  (define (helper n d index k)
    (if (= index k)
	(/ (n k) (d k))
	(/ (n index) (+ (d index) (helper n d (+ index 1) k)))))
  (helper n d 1 k))


;;;  NB.  This procedure uses a helper function to track an additional variable, 'index'.
;;;       'index' keeps track of the current term-level starting from 1 and going to k.
;;;       

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   1000)

;;; An iterative process for cont-frac:

(define (cont-frac-iter n d k)
  (define (helper n d k fraction)
    (if (= 1 k)
	fraction
	(helper n d (- k 1) (/ (n (- k 1)) (+ (d (- k 1)) fraction)))))
  (helper n d k (/ (n k) (d k))))


;;;  Ex. 1.38 p. 71
;;;  Euler's continued fraction expansion for e - 2.
;;;  N(i) = 1 for all i
;;;  D(i) = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8 . . .

(define (euler-n i)
  i)

(define (euler-d i)
  (cond ((= i 1) 1)
	((= i 2) 2)
	((= 0 (remainder i 3)) 1)
	((= 1 (remainder i 3)) 1)
	((= 2 (remainder i 3)) (* 2 (+ (floor (/ i 3)) 1)))))


;;;  e - 2  =~  cont-fract(euler-n, euler-d, depth = k)

(define (approx-e k)
  (exact->inexact
   (+ 2
     (cont-frac euler-n
	     euler-d
	     k))))

;;;  Here is the result :

(approx-e 10000)
;Value: 2.598171741866604

;;;  e =~ 2.7183 so the result does not seem very good to me. 

;;; TO REVIEW.