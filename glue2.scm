;;;  Ex. 2.4 p. 92
;;;  Alternative representation for cons, car, cdr

(define (cement x y)
  (lambda (piece)
    (piece x y)))

(define (primo pair)
  (pair (lambda (x y) x)))

(define (secundo pair)
  (pair (lambda (x y) y)))

(define p2 (cement -10 20))

(primo p2)

(secundo p2)

