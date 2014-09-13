;;;  Ex. 2.36 p. 120
;;;  Let's represent a vector as a sequence of numbers and matrices as 
;;;  a sequence of vectors (rows in a matrix).
;;;  The sequence ((1 2 3 4) (4 5 6 6) (7 8 8 9)) is a matrix with 3 rows
;;;  and 4 columns. Each row is a vector.

(define (dot-product v w)
  (accumulate +
	      0
	      (map * v w)))

(dot-product (list 1 2 3) (list 3 4 5))

(dot-product (list 1 0 0) (list 0 1 0))


;;;  matrix-*-vector returns a vector that is the product of a matrix
;;;  and a vector:

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define m1 (list (list 2 1 1) (list 1 2 1) (list 1 1 2)))

(define v1 (list 2 2 2))

(matrix-*-vector m1 v1)


;;;  transpose of a matrix:

(define (transpose mat)
  (accumulate-n cons
		()
		mat))

;;; the operation is cons so that accumulate joins up 
;;; the first element of each sub-list (vector)
;;; and then does the same for the next elements
;;; in this way, the first vector is the vector of each 
;;; of the first elements of the vectors in the matrix to 
;;; transpose
;;; a drawing makes it easy to see that this is correct

(transpose m1)


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))


(define id (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define m2 (list (list 2 1 5) (list 4 2 3) (list 6 5 1)))

(matrix-*-matrix m1 m2)
