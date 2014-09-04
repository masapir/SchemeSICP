; Factorial takes a positive integer and returns n!

(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))


; Factorial by iterative process in the following recursive procedure bc
; operating function maintains state variables to track state

(define (factoriali n)
  (define (fact-helper product counter)
    (if (= counter 1)
	product
	(fact-helper (* counter product) (- counter 1))))
  (fact-helper 1 n))
