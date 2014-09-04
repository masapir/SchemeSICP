;;; Greatest common divisor of a and b using Euclid's method
;;; Given a/b, GCD(a, b) = GCD(b, r) with r = remainder of a/b

(define (gcd num denom)
  (if (= denom 0) num
      (gcd denom (remainder num denom))))


(define (divides? denom num)
  (= (remainder num denom) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))


;;; Primality test:

(define (prime? n)
  (= (smallest-divisor n) n))
