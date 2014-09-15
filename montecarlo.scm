;;;  Ex. 3.5 p. 228 Monte Carlo integration

;;;  takes a number of trials and a predicate test experiment and returns
;;;  the ratio of times the trial was passed to total number of trials

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(random 10)

(random 10)

(random 5)

(random-in-range 5 15) ; Value: 11.


;;;  estimate-integral uses the monte carlo method to estimate the area within a 
;;;  the curve describe by a function--for example the area within a circle

;;;  estimate-integral takes 6 arguments:

;;;    a function predicate that test whether a given point lies within a function

;;;    you could build the function into the predicate but let's separate it
;;;    the coordinates for 2 points describing an rectangle enclosing the function curve

;;;    the rectangle box will be the space in which random points are chosen to check 
;;;    if they are in the function curve

;;;    the number of trials to run on the monte carlo checker

;;;  for example, a function describing a circle of radius 3 and centered at (5, 7)


;;;  a function that tests whether (x, y) is inside circle centered at (5, 7) with
;;;  radius 3:

(define (in-circle573? x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))

(define (unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(in-circle573? 5 7) ; Value: true

(in-circle573? 25 49) ; Value: false

(define (square x) (* x x))


;;;   

;;;  the box coordinates x1 x2 y1 y2 should be entered in the order:
;;;  bottom left corner, top right corner of the box

(define (estimate-area predicate x1 x2 y1 y2 trials)
  (define (location-test)
    (predicate (random-in-range x1 y1) (random-in-range x2 y2)))
  (let ((box-area (* (- y1 x1) (- y2 x2))))
    (* box-area (monte-carlo trials location-test))))

(estimate-area in-circle573? 2 4 8 10 1000) ; Value 27.75 (area is 28.27)

(estimate-area in-circle573? 2 4 8 10 10000) ; Value is 26.9

(estimate-area in-circle573? 2 4 8 10 1000000) ; Value is 27.024 -- i think there is a mistake

(estimate-area unit-circle? -1.0 -1.0 1.0 1.0 10000)
