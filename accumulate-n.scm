;;;  Ex. 2.36 p. 120
;;;  The procedure accumulate-n similar to accumulate
;;;  but 3rd argument is a sequence of sequences with same
;;;  number of elements
;;;  then accumulate-n behaves like map element-wise

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;;;  NB.  seqs is a list of lists
;;;  mapping car onto seqs creates a list of the first
;;;  element in each list in seqs
;;;  mapping cdr onto seqs creates a list of lists compristing
;;;  the cdrs of each list in seq.
;;;  this is the correct list to look at for consing
;;;  Key is to describe precisley what is the list formed by 
;;;  map
;;;  (cdr seqs) is wrong because it contains all the lists except
;;;  the first list in the list seqs whereas what you want
;;;  is the list of each list in seqs minus the first element
;;;  You need to map cdr onto seqs for that.

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(accumulate-n + 0 s)

(map car s)

(accumulate + 0 (map car s))
