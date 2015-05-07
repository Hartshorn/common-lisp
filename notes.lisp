(defun count-instances (obj lsts) 
  (labels ((instances-in (lst) 
    (if (consp lst)
      (+  (if (eq (car lst) obj) 1 0)
      (instances-in (cdr lst)))
      0)))
    (mapcar #'instances-in lsts)))
    
(defun doublem (lst) (mapcar (lambda (x) (* 2 x)) lst))

;; define a reload function to shorten the time it takes to load changes to this file
(defun r () (load "notes.lisp"))

#|
 | Take function - returns n values from lst
 |#
;; This is also a comment
(defun take (n lst)
  (if (or (eq nil lst) (eq n 0)) 
    nil
    (cons (car lst) (take (- n 1) (cdr lst)))))

;; takes a list and prints out the values
(defun prlst (lst)
  (loop for i in lst do
	(print i)))

;; some cool looping
(defun growing_lists ()
  (loop for x from 1 to 10 collect
	(loop for y from 1 to x collect y)))
