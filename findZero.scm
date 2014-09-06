;;;
;;;  Half-interval method to solve f(x) = 0 where f is continuous
;;;
;;;  Given a and b s.t.  f(a) < 0 < f(b), f must have a zero in [a, b]
;;;  To find zero, let mid-point = (a + b)/2.0 :
;;;  if f(mid-point) > 0 then zero is in [a, mid-point]
;;;  if f(mid-point) < 0 then zero is in [mid-point, b]
;;;  call find function on the smaller interval

(define (find-zero f a b)
  (let ((mid-point (/(+ a b) 2.0)))
    (if (< (abs (f mid-point)) 0.001)
	mid-point
	(if (> (f mid-point) 0)
	    (find-zero f a mid-point)
	    (find-zero f mid-point b)))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (close-enough? a b)
  (< (abs (- b a)) 0.0001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value) (search f neg-point midpoint))
		((negative? test-value) (search f midpoint pos-point))
		(else midpoint))))))
