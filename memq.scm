;;;  memq takes a symbol and a list
;;;  returns false if symbol not in list
;;;  returns list beginning with symbol if
;;;  symbol in list:

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))


(memq 'apple '(x (apple sauce) y apple pear))  ;  Value: (apple pear)
