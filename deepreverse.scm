;;;  Ex. 2.27 p. 110
;;;  Modify reverse of 2.18 to produce deep-reverse
;;;  deep-reverse takes a list and returns a list
;;;  with elements reversed and all sublist elements
;;;  reversed.


(define (deep-reverse l)
  (if (not (list? l))
      l
      (reverse (map deep-reverse l))))

;;;  NB.  That by applying its procedure argument to its list
;;;  elements, the map function effects the recursion step.
;;;  So while it might not look like the procedure is advancing
;;;  at each iteration, it is because map is looking into the list
;;;  elements--the recursion here is to explore the elements of a list.
;;;  If an element is a list, then that list in turn gets deep reversed.
;;;  In conjunction with this step, the elements in the first list are reversed.
;;;  The process reverses list elements and calls deep-reverse on each list element
;;;  through map for the recursion step down.

;;;  The key to expressing this procedure is to understand clearly the structure
;;;  of the data argument to process. That enables you to describe the steps of the
;;;  process necessary to walk down the data structure.

;;;  Ex. 2.27 is a good illustration of recursion.

(define tl (list 1 (list 2 3) (list 4 5) 6))

(deep-reverse tl)
  
(cdr (list 1 2 3))

(list 1 (list 2 3) (list 4 5) 6) 
