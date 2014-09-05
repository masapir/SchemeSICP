;;;  Using procedures as arguments to generalize functions (SICP p. 57)

;;; Summation of terms from a to b with general next function :

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))


;;;  Incrementing function :

(define (inc j)
  (+ j 1))

  
;;; Sum of cubes of integers [a, b]

(define (sum-cubes a b)
  (sum cube a inc b))


;;; Identity function :

(define (identity x)
  x)

(define (sum-integers a b)
  (sum identity a inc b))

;;; pi-sum = pi / 8  =  1/1*3  +  1/5*7  +  1/9*11  +  1/13*15  +  1/17*19 + . . . 
;;; pi is approximated by (* 8 pi-sum 1 n) for large n
 
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
