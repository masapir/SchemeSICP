;;; Exponentiate b^n

(define (expt b n)
  (define (expt-helper counter product)
    (if (= counter 0) product
	(expt-helper (- counter 1) (* product b))))
  (expt-helper n 1))

(define (expt-fast b n)
  (cond ((= n 0) 1)
	((even? n) (square (expt-fast b (/ n 2))))
	(else (* b (expt-fast b (- n 1))))))


;;; Exponentiate faster with an iterative process
;;; SICP p. 46 ex. 1.16
;;; uses (b^n/2)^2 = (b^2)^n/2

(define (expti-fast base n)
  (define (expti-driver base n invariant) ; invariant*base^n = 1 i.e. captures invariant
    (cond ((= n 0) invariant)
	  ((even? n) (expti-driver (square base) (/ n 2) invariant))
	  (else (expti-driver base (- n 1) (* invariant base)))))
  (expti-driver base n 1))
