;;;  filtered-accumulate accumulates only function values that pass the specified filter :

(define (filtered-accumulate combiner null-value f filter a next b)
  (define (filter-proc x)
    (if (filter x)
	x
	null-value))
  (if (> a b)
      null-value
      (combiner (f (filter-proc a)) (filtered-accumulate combiner null-value f filter (next a) next b))))

(define (product-even n)
  (filtered-accumulate * 1 identity even? 1 inc n))
      

;;;
;;;  sum of squares of prime numbers (prime?) in interval [a, b]
;;;

(define (sum-sq-pr a b)
  (filtered-accumulate + 0 square prime? a inc b))


;;;
;;;  product of positive integers i less than n s.t. GCD(i, n) = 1
;;;

(define (r-prime? i n)
  (= (gcd i n) 1))

(define (prod-rprime n)
  (define (r-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity r-prime? 1 inc n))
