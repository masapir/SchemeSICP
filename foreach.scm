(define (scale-list l factor)
  (if (null? l)
      ()
      (cons (* (car l) factor)
	    (scale-list (cdr l) factor))))

(scale-list (list 1 2 3 4 5) 10)


;;;  Ex. 2.21 p. 106

;;; returns a list of the squares of each item in input list:

(define (square-list1 l)
  (if (null? l)
      ()
      (cons (* (car l) (car l)) (square-list1 (cdr l)))))

(square-list1 (list 1 2 3 4 5 6))

(define (square-list2 l)
  (map square l))

(define (square x)
  (* x x))

(square-list2 (list 1 2 3 4 5 6 7))

;;;  Ex. 2.23 p. 107

;;;  define for-each to take a procedure and a list
;;;  and to apply the procedure to each element in 
;;;  the list

(define (for-each proc l)
  (define (iterator proc l counter)
    (cond ((= counter 0) (newline))
	  (else
	   (proc (car l))
	   (iterator proc (cdr l) (- counter 1)))))
  (iterator proc l (length l)))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))

(cons (list 1 2) (list 3 4))

(cons (list 1 2 ) (cons 3 4))

;;;  Data structures that lend themselves
;;;  to recursion (by being structurally recursive)
;;;  are easier to work on and access (using recursive
;;;  processes) Why? Because recursion allows us
;;;  to express processes concisely and effectively
;;;  and it allows us to explore unlimited spaces.

;;;  Key idea here is recursive structures (because recursion
;;;  is such a powerful paradigm for expressing processes).

;;; pair? tests whether its argument is a pair
;;; use pair? to implement count-leaves which
;;; counts the elements in a tree structure

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(count-leaves (list (list 2 3 5 3) 6 (list 4 6 4)))

(list 1 (list 2 (list 3 4)))

(cdr (car (cdr (cdr (list 1 2 (list 5 7) 9)))))

(car (car (list (list 7))))
