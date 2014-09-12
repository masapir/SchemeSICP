;;;  Ex. 2.34 p. 119  ********
;;;  Evaluating a polynomial in x at a given value of x can be formulated as an accumulation.
;;;  a_n * x^n  +  a_n-1 * x^(n-1)  + ... +  a_1 * x  +  a_0  =
;;;  (... (a_n * x  +  a_(n-1) * x  + ...  + a_1) * x  +  a_0
;;;  start with the nth coefficient; multiply by x then add next coefficient; multiply by x
;;;  all the way to the last coefficient a_0.


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms  x) this-coeff)) 
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


;;;  Ex. 2.35 p. 120
;;;  Redefine count-leaves as an accumulation

(define (count-leaves2 t)
  (accumulate (lambda (x y) (+ 1 y))
	      0
	      (enumerate-tree t)))

(define (count-leaves3 t)
  (accumulate (lambda (x y) (+ 1 y))
	      0
	      (map (lambda (x) 1) (enumerate-tree t)))) ; not really doing anything different from count-leaves2

