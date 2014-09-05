;;;  Definite integral of f from a to b
;;;  integral f from a to b = dx*[f(a + dx/2)  +  f(a + dx + dx/2)  +  f(a + 2dx + dx/2)  +  ]
;;;  The second factor in that product is a sum of values of f at regular intervals (dx)
;;;  for dx small


(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx  b)))


;;;  Simpson rule approximation to integral
;;;
;;;  integral of f over a to b is approximately
;;;  h/3  *  [y0 + 4y1 + 2y2 + 4y3 + 2y4 + 4y5 + . . . + yn]
;;;  with n even and h = (b - a)/n
;;;  yk = f(a + kh)

(define (simpson f a b n)
  (define (s-next x)   
    (+ x (/ (- b a) n)))
  (define (s-term x)               
    (cond ((or (= x a) (= x b)) (f x))                        
	  ((odd? (/ (* n (- x a)) (- b a))) (* 4 (f x)))      
	  ((even? (/ (* n (- x a)) (- b a))) (* 2 (f x)))))   
  (* (/ (- b a) (* n 3)) (sum s-term a s-next b)))           


;;;  Notes on Simpson :

;;;  s-next returns the next input value x (to determine the next function value for the next
;;;  term in sum
;;;
;;;  s-term returns the next term in the sum with the correct Simpson coefficient
;;;  Conditional : 
;;;     end points on the interval (a and b) return f(x) (coeff is 1)
;;;     odd terms return 4*f(x)
;;;     even terms return 2*f(x)

;;;  Loop process :
;;;     the function sum moves the process through successive
;;;     x-value steps and stops when the x-value reaches end point
;;;     b by returning 0
;;;     I.e., the loop process is achieved in the structure of this function  
