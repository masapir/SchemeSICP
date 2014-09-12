;;;  Ex. 2.33 p. 119
;;;  


;;; chart behaves like map:

(define (chart p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(chart square (list 1 2 3 4 5)) 

;;; glue behaves like append:

(define (glue seq1 seq2)
  (accumulate cons seq2 seq1))

(glue (list 1 2 3) (list 4 5 6))

(append (list 1 2 3) (list 4 5 6))


;;; size behaves like length:

(define (size sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


(size (list 1 2 3 4 5))

(length (list 1 2 3 4 5))