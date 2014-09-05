;;;
;;;  (accumulate combiner null-value term a next b)
;;;  the function accumulate is a generalization of sum and product :
;;;

(define (accumulate combiner null-value f a next b)
  (if (> a b)
      null-value
      (combiner (f a) (accumulate combiner null-value f (next a) next b))))

(define (suma f a next b)
  (accumulate + 0  f a next b))

(define (producta f a next b)
  (accumulate * 1 f a next b))

;;;  iterative accumulate :

(define (accumulate-iter combiner null-value f a next b)
  (define (accumulate-helper cume f a next b)
    (if (> a b)
	cume
	(accumulate-helper (combiner cume (f a)) f (next a) next b)))
  (accumulate-helper null-value f a next b))

(define (sumai f a next b)
  (accumulate-iter + 0 f a next b))

(define (productai f a next b)
  (accumulate-iter * 1 f a next b))
