(defun rc ()
  (load "prob23.lisp"))

(defun count1 ()
  (let ((counter 0))
    (lambda () (incf counter))))


(defun map! (fn lst)
  (loop for i in lst collect (funcall fn i)))

(defun huh? ()
  (apply #'* (loop for i in (loop repeat 100 collect (random 10)) collect (1+ i))))

(defun n! (n)
  (apply #'* (loop for i from 1 to n collect i)))

(defun prn! (lst)
  (loop for i in lst do (format t "~d~t~d~%" (car i) (cdr i))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun divisors (n)
  (let ((upto-n (loop for i from 1 to (1- n) collect i)))
    (loop for i in upto-n if (eq (mod n i) 0) collect i)))

(defun sum-div (n)
  (apply #'+ (divisors n)))

(defun p-num? (n)
  (eq n (sum-div n)))

(defun a-num? (n)
  (< n (sum-div n)))

(defun d-num? (n)
  (> n (sum-div n)))

(defun flatten (lst)
  (unless (null lst)
    (append (car lst) (flatten (cdr lst)))))

(defun abuns-upto (n m)
  (loop for i from n to m if (a-num? i) collect i))

(defun abun-sums (n m)
  (let ((as (abuns-upto n m)))
    (remove-duplicates 
      (flatten 
	(loop for i in as collect 
	      (mapcar (lambda (x) (+ x i)) 
		      (loop for i in as collect i)))))))


(defvar n1 (abun-sums 0 1000))
(defvar n2 (abun-sums 1001 2000))
(defvar n3 (abun-sums 2001 3000))
(defvar n4 (abun-sums 3001 4000))
(defvar n5 (abun-sums 4001 5000))
(defvar n6 (abun-sums 5001 6000))
(defvar n7 (abun-sums 6001 7000))
(defvar n9 (abun-sums 7001 8000))
(defvar n10 (abun-sums 8001 9000))
(defvar n11 (abun-sums 9001 10000))
(defvar n12 (abun-sums 10001 11000))
(defvar n13 (abun-sums 11001 12000))
(defvar n14 (abun-sums 12001 13000))
(defvar n15 (abun-sums 13001 14000))

;; this needs to go higher - I think to 28000?
;; that's why the answer was the same before. . .
(defvar vals (loop for i from 0 to 28000 
		   if (not (or 
			     (member i n1)
			     (member i n2)
			     (member i n3)
			     (member i n4)
			     (member i n5)
			     (member i n6)
			     (member i n7)
			     (member i n8)
			     (member i n9)
			     (member i n10)
			     (member i n11)
			     (member i n12)
			     (member i n13)
			     (member i n14)
			     (member i n15))) collect i))

(defvar answer (apply #'+ vals))
