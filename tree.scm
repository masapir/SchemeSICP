;;;  Ex. 2.30 p. 112
;;;  Define square-tree ~ square-list

(define (square x)
  (* x x))

(define (square-tree1 tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree1 (car tree)) (square-tree1 (cdr tree))))))

(square-tree1 (list 1 (list 2 (list 3 4) 5 (list 6 7))))

;;; use map for the procedure; think about the list as a list
;;; of trees that you are mapping over
;;; trees are a useful data structure convention

(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree2 sub-tree)
	     (square sub-tree)))
       tree))

(square-tree2 (list 1 (list 2 (list 3 4) 5 (list 6 7))))


;;;  Ex. 2.31 p. 113
;;; tree-map -- captures tree map pattern generally:

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree3 tree) (tree-map square tree))

(square-tree3 (list 1 (list 2 (list 3 4) 5 (list 6 7))))
