;;;  Ex. 2.2 p. 89
;;;  Represent line segments in the plane
;;;  Each segment is represented by a pair of points (x, y)
;;;  make-segment is the constructor
;;;  start-segment is a selector returning the start point
;;;  end-segment is a selector returning the end point
;;;  make-point is a constructor that makes a point from 2 numbers
;;;  x-point is a selector returning the x coordinate of a point
;;;  y-point is a selector returning the y coordinate of a point.
;;;
;;;  Procedure midpoint-segment takes a line segment as an argument
;;;  and returns its midpoint

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))

(define origin (make-point 0 0))

(define x-one (make-point 1 0))

(define y-one (make-point 0 1))

(define t (make-segment x-one y-one))

(print-point (midpoint-segment t))
