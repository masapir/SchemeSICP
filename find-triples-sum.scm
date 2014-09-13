;;;  Ex. 2.41 p. 124.

(define (find-triples-sum n s)
 (filter (lambda (seq)
	   (= (accumulate + 0 seq) s))
	 (unique-triples n)))
  

(define (unique-triples n)
  (accumulate append
	      ()
	      (flatmap (lambda (i)
		     (map (lambda (j)
			    (map (lambda (k)
				   (list i j k))
				 (enumerate-interval 1 (- i 2))))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

(unique-triples 10)

(find-triples-sum 10 15)
