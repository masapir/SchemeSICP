;;;  Ex. 1.39 p. 72.
;;;  Lambert's continuous fraction to approximate tan(x)
;;;  tan(x)  =  x / (1 - (x^2 / (3 - (x^2 / (5 - . . .
;;;

(define (lambert-tan x k)
  (define (n i)
    (if (= 1 i)
	x
	(- (* x x))))
  (define (d i)
    (if (= i 1)
	1
	(- (* 2 i) 1)))
  (cont-frac-iter n d k))

(lambert-tan (/ (* 2 (asin 1)) 4) 10)

;Value: 1.
