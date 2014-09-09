;;;  Building Abstractions with Data
;;;

; representing rational numbers

(define (make-rat x y)
  (cons x y))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(make-rat 7 8)

(denom (make-rat 7 8)) 

(numer (make-rat 7 8))

(define test (make-rat 4 5))

(denom test)


;;;  Arithmetic operators for the rationals data structure:

(define (add-rat x y)
  (make-rat (+ (* (denom y) (numer x))
	       (* (denom x) (numer y)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (neg-rat x)
  (make-rat (- (numer x)) (denom x)))

(define t (make-rat 2 3))

(neg-rat t)

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))
      

;;;  a function to display rationals :

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (exact->inexact (/ (numer x) (denom x))))

(print-rat (make-rat 11 12))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(print-rat (div-rat one-half one-third))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((or (and (< n 0) (< d 0))
	       (and (> n 0) (< d 0)))
	   (cons (/ (- n) g) (/ (- d) g)))
	  (else (cons (/ n g) (/ d g))))))

(print-rat (add-rat one-third one-third))

(make-rat -1 3)

(numer (make-rat -1 3))

(make-rat -2 -6)

(make-rat 5 -10)

(make-rat 3 9)
