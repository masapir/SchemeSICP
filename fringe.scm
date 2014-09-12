;;;  Ex. 2.28

;;;  Write a procedure fringe that takes as argument a tree (represented as a list)
;;;  and returns a list whose elements are all the leaves of the tree arranged in 
;;;  left-to-right order. For example,
;;;  (define x (list (list 1 2) (list 3 4)))
;;;  (fringe x) -> (1 2 3 4)
;;;  (fringe (list x x)) -> (1 2 3 4 1 2 3 4)

(define (fringe l)
  (cond ((null? l)
	 l)
	((not (list? (car l)))
	 (append (list (car l)) (fringe (cdr l))))
	(else
	 (append (fringe (car l)) (fringe (cdr l))))))
       


(define x (list (list 1 2) (list 3 4)))

x

(fringe x)

(list? ())
(null? ())
(append (list 1 2 3) (list 7 8 9))

(append (list 1) (list 2 4))

(list? ())

