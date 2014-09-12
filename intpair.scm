;;;  Ex. 2.5 p. 92
;;;  Show that we can represent pairs of non-negative integers using
;;;  only numbers and arithmetic operations if we represent the
;;;  pair a and b as the integer that is the product (2^a)*(3^b).
;;;  Give the corresponding definitions of the procedures cons, 
;;;  car, and cdr.


;;;  the function int-pair takes two positive integers
;;;  and pairs them using the formula above
;;;  this is the pair constructor

(define (int-pair a b)
  (* (expt 2 a) (expt 3 b)))


;;;  factor selectors
;;;
;;;  the first selector returns the first integer in a given
;;;  pair
;;;  to do this, the selector strips out the threes by 
;;;  recursively dividing out three until there are no
;;;  more threes
;;;  then the selector counts how many twos are in the 
;;;  remaining number
;;;  the selector for the second item in the pair works
;;;  the same way but flipping two for three and
;;;  three for two


(define (first-factor pair)
  (define (three-strip n)
    (if (not (= (modulo n 3) 0))
	n
	(three-strip (/ n 3))))
  (define (two-count m)
    (cond ((< m 2) 0)
	  ((= m 2) 1)
	  (else (+ 1 (two-count (/ m 2))))))
  (two-count (three-strip pair)))

(define (second-factor pair)
  (define (two-strip n)
    (if (not (= (modulo n 2) 0))
	n
	(two-strip (/ n 2))))
  (define (three-count m)
    (cond ((< m 3) 0)
	  ((= m 3) 1)
	  (else (+ 1 (three-count (/ m 3))))))
  (three-count (two-strip pair)))

(define int-pair1 (int-pair 6 32))

(first-factor int-pair1)

(second-factor int-pair1)


;;;  Ex. 2.6 p. 93
;;;  representing numbers using only procedures

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
