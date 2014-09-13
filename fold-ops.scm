;;;  Ex. 2.38 p. 121
;;;  The accumulate procedure is also known as fold-right because it combines
;;;  the first element of the sequence with the result of combining all the elements to the right
;;;  fold left . . . .

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	       (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 3))

(fold-right / 1 (list 2 3))

(fold-right / 1 (list 1 2))

(fold-left / 1 (list 1 2))

(fold-left / 1 (list 1 2 3))

(fold-left / 1 (list 2 3))

(fold-right / 1 (list 2 3))

(fold-right / 1 (list 1 2 3 4))
      
(fold-right list () (list 1 2 3))

(fold-left list () (list 1 2 3))



(fold-right * 1 (list 2 3 4 5))

(fold-left * 1 (list 2 3 4 5))

;;; same for +

(fold-right + 0 (list 1 2 3 4))

(fold-left + 0 (list 1 2 3 4))


;;;  Ex. 2.39 p. 122
;;;  complete reverse using fold-right and fold-left:

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(reverse (list 1 2 3 4))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))

(reverse2 (list 1 2 3 4))

(append (list 1 2) (list 3 4))
