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

(defun factors (n)
  (let ((upto-n (loop for i from 1 to n collect i)))
    (loop for i in upto-n if (eq (mod n i) 0) collect i)))

(defun sum-div (n)
  (apply #'+ (factors n)))

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
		      
(defun abun-sums! (n m)
  (let ((as (abuns-upto n m)))
    (loop for i in as collect (+ i i))))
    
(defun is_member (n lst)
    (if (member n lst) t))

;(defvar n (abun-sums! 0 15000))

;(defvar vals (loop for i from 0 to 28123 if (not (member i n)) collect i))

;(defvar answer (apply #'+ vals))

; (defvar answer 
;   (apply #'+ 
;     (loop for i from 0 to 28122 
;       if (not (member i (abun-sums! ))) 
;         collect i)))


(defun prime-factors (n)
  (let ((factors (factors n)))
    (loop for i in factors if (eq 2 (length (factors i))) collect i)))

(defun fib (n)
  (if (<= n 2)
    1
    (+ (fib (1- n)) (fib (- n 2)))))

























