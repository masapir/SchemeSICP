;;;  flatmap creates a list by applying proc to each item in list seq:
;;;  seq is a list of lists
;;;  map works on a list of primitives

(flatmap (lambda (i) (+ 1 i)) (list (list 1) (list 2) (list 3) (list 4) (list 5)))

 
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))



;;;  a procedure to generate list of integers i, j with j < i and 
;;;  i + j prime; i, j both < n

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(prime-sum-pairs2 20)


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(permutations (list 1 2 3 4))
