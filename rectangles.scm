;;;  Ex. 2.3 p. 90
;;;  Implement a representation for rectangles in a plane.  
;;;  Use ex. 2.2.
;;;  Create perimeter
;;;  Create area
;;;  Create second rectangle representation
;;;  perimeter and area should work with both representations

;;;  we will represent length to be the longer side of the rectangle
;;;  and height to be the shorter side

(define (area rectangle)
  (* (length (rec-width rectangle)) (length (rec-height rectangle))))

(define (perimeter rectangle)
  (+ (* 2 (rectangle-height rectangle)) (* 2 (rectangle-length rectangle))))


(define (length segment)
  (let ((a (- (x-point (end-segment segment)) (x-point (start-segment segment))))
	(b (- (y-point (end-segment segment)) (y-point (start-segment segment)))))
    (sqrt (+ (square a) (square b)))))

(define (square x)
  (* x x))


;;; rectangle constructor from two adjacent segments:

(define (make-rectangle height width)
  (cons height width))

;;; rectangle selectors height width
 
(define (rec-height rectangle)
  (car rectangle))

(define (rec-width rectangle)
  (cdr rectangle))
  

(define c1 (make-point 0 0))
(define c2 (make-point 12 0))
(define c3 (make-point 0 15))
(define rw (make-segment c1 c2))
(define rh (make-segment c1 c3))
(define box (make-rectangle rw rh))

(area box)

(length rw)
(length rh)
