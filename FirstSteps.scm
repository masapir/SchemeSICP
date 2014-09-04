;;; From Abelson & Sussman, Structure & Interpretation of Computer Programs

;;; Basic Scheme syntax and special forms

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

;;; cond to test a condition with different outcomes

(define (absp x)
  (cond ((< x 0) (- x))
	((> x 0) x)
	((= x 0) 0)))

;;;  'else' form

(define (abs x)
  (cond ((< x 0) (- x))
	(else x)))

;;; 'if' special form

(define (abswif x)
  (if (> x 0)
      x
      (- x)))

;;; logical operators: and or not (operators precede operands)

(define (logic-ops-test x y z)
  (cond ((and (= x y) (not (= x z))) 1)
	((or (< x z) (< x y)) 2)
	((not (< y z)) 3)))

(define (>= x y)
  (or (> x y) (= x y)))


;;; Operators may be compound expressions:

(define (a-plus-abs-b a b)
  ((if (< b 0) - +) a b))
