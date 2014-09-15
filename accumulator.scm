;;;  Ex. 3.1 p. 224
;;;  an accumulator is a procedure that is called repeatedly with a single numeric argument and
;;;  accumulates its arguments into a sum. Each time it is called, it returns the currently
;;;  accumulated sum. Write a procedure make-accumulator that generates accumulators, each maintaining
;;;  an independent sum. The input to make-accumulator should specify the initial value of the sum;
;;;  for example 
;;;  (define A (make-accumulator 5))
;;; 
;;;  (A 10) ; value: 15
;;;  (A 10) ; value: 25


(define (make-accumulator amount)
  (let ((sum amount))
    (lambda (next)
      (set! sum (+ sum next))
      sum)))

(define a1 (make-accumulator 3))

(a1 10)


;;; make-monitored

(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?)
	     calls)
	    ((eq? arg 'reset-count)
	     (set! calls 0))
	    (else
	     (f arg)
	     (set! calls (+ 1 calls))
	     calls)))))

;;;  if you will be changing the value of a variable from a procedure
;;;  then you want the variable to be outside the scope of the procedure call
;;;  otherwise the variable gets created everytime you call the procedure
;;;  that's why you define calls with let before creating the procedure

(define (t1 x)
  (+ x 10))

(define p1 (make-monitored t1))

(p1 'how-many-calls?)

(p1 4)

(p1 'how-many-calls?)

(p1 'reset-count)
