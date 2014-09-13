;;;  Symbolic differentation p. 147

;;;  derive returns derivative of exp w.r.t. var

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unknown expression type -- DERIV" exp))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))



(deriv '(+ x 3) 'x)  ;  Value: (+ 1 0)




(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(deriv '(* (* x y) (+ x 3)) 'x)  ;Value 15: (+ (* (* x y) 1) (* (+ (* x 0) (* 1 y)) (+ x 3)))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(deriv '(+ x 3) 'x) ; Value: 1

(deriv '(* x y) 'x) ; Value: y


;;  Ex. 2.56 p. 150
;;  Extend deriv to accommodate d(u^n)/dx  =  n*u^(n-1)*(du/dx)


(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))
	 
	 ;;;  Selectors, predicate, constructor:

	 ;;;  exponential? checks if exp has form of an exponential
	 ;;;  exponand returns the power to which base is raised in exp
	 ;;;  make-exponential returns expression of base to power of exponand
	 ;;;  base returns base of exp

	 ;;;  an exponential will have the form: (^ base power)


	 (define (exponentiation? exp)
	   (and (pair? exp) (eq? (car exp) '^)))

	 (define (base exp)
	   (cadr exp))

	 (define (exponent exp)
	   (caddr exp))

	 (define (make-exponentiation base power)
	   (cond ((=number? power 0) 1)
	 	((=number? power 1) base)
	 	(else (list '^ base power))))


	 ;;; test

	 (deriv '(^ x 3) 'x)  ;Value 16: (* 3 (^ x 2))

	 (deriv '(^ x 3) 'x)

	 (deriv '(^ x 1) 'x)

	 (deriv '(^ x 5) 'x)

	 (deriv '(* 5 (^ x 6)) 'x)
	 
