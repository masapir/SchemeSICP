;;;  Ex. 2.40 p. 124 ;; alternative form to loop constructs:
;;;  unique-pairs takes an integer n and returns a sequence of pairs
;;;  (i j), 1 lte j lt i lte n. use unique-pairs in prime-sum-pairs

(define (unique-pairs n)
  (accumulate append
	      ()
	      (map (lambda (i)                              ; first map is like outer loop
		     (map (lambda (j) (list i j))               ; inner map is like inner loop
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

(unique-pairs 4)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
       

(prime-sum-pairs 20)
