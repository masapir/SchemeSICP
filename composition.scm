;;;  Ex. 1.41 p. 77
;;;  Define a procedure double that takes a procedure of one argument as 
;;;  argument and returns a procedure that applies the original procedure twice.
;;;  For example, if inc is a procedure that adds 1 to its argument, then (double inc)
;;;  should be a procedure that adds 2.
;;;  What value is returned by (((double (double double)) inc) 5)?  ans. 21.

(define (double op)
  (lambda (x) (op (op x))))

(define (square x)
  (* x x))

((double square) 3)

(define (inc i)
  (+ i 1))

(((double (double double)) inc) 5)

;;; Value: 21


;;;  Ex. 1.42 p. 77
;;;  Let f and g be two one-argument functions. 
;;;  The composition f after g is defined as
;;;  x -> f(g(x)).
;;;  Define a procedure compose that implements composition.
;;;  For example, if inc is a procedure that adds 1 to its argument
;;;  ((compose square inc) 6) returns 49 : 
;;;  (inc 6) yields 7
;;;  (square 7) yields 49.


(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;;  value: 49


;;;  Ex. 1.43 p. 77
;;;  If f is a numerical function and
;;;  n is a positive integer, then we can
;;;  form the nth repeated application of f,
;;;  which is defined to be the function whose
;;;  value at x is f(f(. . .(f(x)) . . .)).
;;;  For example, if f: x --> x + 1,
;;;  then the nth repeated application of f is
;;;  the function x --> x + n.
;;;  If f is the operation of squaring a number
;;;  then the nth repeated application of f is
;;;  x --> x^(2^n)
;;;  Write a procedure that takes as inputs a 
;;;  procedure that computes f and a positive
;;;  number n and returns a procedure that computes
;;;  the nth repeated application of f.
;;;  Your procedure should be able to be used as follows:
;;;  ((repeated square 2) 5) yields 625

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))


((repeated square 2) 5)
