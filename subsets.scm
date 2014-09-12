;;;  Ex. 2.32 p. 113
;;;  We can represent a set as a list of distinct elements and we can represent the set of all subsets
;;;  of the set as a list of lists. For example, if the set is (1 2 3) then the set of all subsets
;;;  is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
;;;  Complete the following definition of a procedure that generates the set of subsets of a set
;;;  and give a clear explanation of why it works:



(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest
		(map (lambda (e)
		       (append (list (car s)) (list e)))
		     rest)))))

(define s (list 1 2 3))

(subsets s)

;;;  Explanation: 
;;;  The set A of all subsets of S is
;;;  The set P of all subsets of S - {e}
;;;  union with the set R = the set of
;;;  sets obtained by adding e to each
;;;  subset of P.
;;;  This is a recursive definition of S that we can
;;;  express as in the procedure subsets.


(subsets (list 1 2 3 4 5 6 7 8))		     


;;; 

(define (sum-odd-squares1 tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares1 (car tree))
		 (sum-odd-squares1 (cdr tree))))))

(sum-odd-squares1 (list 1 2 3 4 (list 5 6 7) (list 8 (list 9 10)))) ; Value: 165.

(define (even-fibs1 n)
  (define (next k)
    (if (> k n)
	()
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 7)

(fib 34)

(even-fibs1 15)

(define (square x)
  (* x x))

(map square (list 1 2 3 4 5 6 7 8))


;;; a filter procedure p. 115

(define (filter predicate sequence)
  (cond ((null? sequence) ())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

;;; an accumulator:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate cons () (list 1 2 3 4 5))


;;;  an interval enumerator

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 20)


(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 3 (list 4 5 6) (list 7)) 8 9))

(define (sum-odd-squares2 tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(sum-odd-squares2 (list 1 2 (list 3 4 (list 5 6)) 7 8 9))

(define (even-fibs n)
  (accumulate cons
	      ()
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(even-fibs 20)
