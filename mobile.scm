;;;  Ex. 2.29 p. 111
;;;  A binary mobile consists of 2 branches, a left branch and a right branch
;;;  Each branch is a rod of a certain length from which hangs either a weight
;;;  or another binary mobile. We can represent a binary mobile using compound
;;;  data by constructing it from two branches:

(define (make-mobile left right)
  (list left right))

;;;  A branch is constructed from a length (a number) and a structure (number for weight
;;;  or another mobile:

(define (make-branch length structure)
  (list length structure))

;;;  Selectors:

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))                ;;; cadr rather than cdr bc constructor used list rather than cons !!!

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;;;  Note that if the constructor used cons instead of list then
;;;  the selector would use cdr instead of cadr


;;;  total-weight returns the total weight of a mobile


;;; branch-weight is a procedure to support total-weight
;;; branch-weight takes a branch and returns its weight
;;; NB.  be careful thinking through the structure
;;;  of the data being processed.
;;;  Here a branch is a list of a two items:
;;;  the first item is a length, which is a number
;;;  the second item is a structure that can be either
;;;  a number corresponding to a weight or another 
;;;  list corresponding to a mobile. A mobile in turn
;;;  is a list of 2 branches, left and right.
;;;  This means that if the branch structure is a list 
;;;  then it is a mobile.
;;;  If branch structure is a mobile, then you apply 
;;;  left-branch and right-branch to that mobile (branch-structure branch)
;;;  to get the sub-branches. Be careful not to be distracted by nomenclature of arguments
;;;  Think through the data structure carefully to apply selectors in the correct order.


(define (branch-weight branch)
  (if (not (list? (branch-structure branch)))
      (branch-structure branch)
      (+ (branch-weight (left-branch (branch-structure branch)))
	 (branch-weight (right-branch (branch-structure branch))))))

;;; tests for branch-weight:

(define b11 (make-branch 1 1))

(branch-weight b11)

(define b12 (make-branch 1 2))

(branch-weight b12)

(define m11 (make-mobile b11 b11))

m11

(define b3 (make-branch 1 m11))

b3

(branch-weight b3)


;;; total-weight takes a mobile and returns its total weight
;;; a mobile is a list of 2 items: a left branch and a right branch

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
  


;;; tests

(define m2 (make-mobile b12 b3))

(total-weight m11) ; Value: 2 correct.

(total-weight m2) ; Value: 4 correct

(define m3 (make-mobile b12 b12))

(total-weight m3)

(define m4 (make-mobile b3 b3))

(total-weight m4)


;;;  a mobile is balanced if torque applied by top-left branch is equal to torque
;;;  applied by top-right branch (torque  =  length x weight)
;;;  AND if each sub-mobile is balanced
;;;  Design a predicate to test whether a mobile is balanced

(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (define (end-branch? branch)
    (not (list? (branch-structure branch))))
  (if (end-branch? (left-branch mobile))
      (if (end-branch? (right-branch mobile))
	  (= (branch-torque (left-branch mobile))
	     (branch-torque (right-branch mobile)))
	  (and (= (branch-torque (left-branch mobile))
		  (branch-torque (right-branch mobile)))
	       (balanced? (branch-structure (right-branch mobile)))))
      (if (end-branch? (right-branch mobile))
	  (and (= (branch-torque (left-branch mobile))
		  (branch-torque (right-branch mobile)))
	       (balanced? (branch-structure (left-branch mobile))))
	  (and (= (branch-torque (left-branch mobile))
		  (branch-torque (right-branch mobile)))
	       (balanced? (branch-structure (left-branch mobile)))
	       (balanced? (branch-structure (right-branch mobile)))))))


;;; build some mobiles and test these functions, then write a function that will assign weight
;;; to the nodes in a specified mobile structure so that the mobile is balanced.

;;;  see if you can simplify the balanced? code

;;; tests

(balanced? m11)

(balanced? m2)

(balanced? m3)

(balanced? m4)

(define m5 (make-mobile (make-branch 4 m3) (make-branch 2 m4)))

(balanced? m5)
