(defun r ()
  (load "dstuct.lisp"))

(defstruct lst cnt vals)

(defun lst_add (nbr lst)
  (unless (null nbr)
    (incf (lst-cnt lst))
    (push nbr (lst-vals lst))))

(defun lst_pop (lst)
  (decf (lst-cnt lst))
  (pop (lst-vals lst)))

(defun lst_add_list (l lst)
  (loop for i in l do (lst_add i lst)))

(defun lst_size (lst)
  (lst-cnt lst))

(defun lst_index (n lst)
  (nth (- (lst-cnt lst) (1+ n)) (lst-vals lst)))

(defun lst_splice_into (index value lst)
  (when (<= index (lst_size lst))
    (let ((vals (lst-vals lst))
	  (first_half  (loop 
			 for i upfrom 1 to index 
			 collect (lst_index i lst)))
	  (second_half (loop 
			 for i upfrom (1+ index) to (lst_size lst) 
			     collect (lst_index i lst))))
      (progn
	(lst_reset lst)
	(let ((b (reverse first_half))) (progn (push value b) (setq first_half b)))
	(lst_add_list (reverse second_half) lst)
	(lst_add_list first_half lst))))
  (setf (lst-vals lst) (reverse (lst-vals lst))))

(defun lst_reset (lst)
  (progn
    (setf (lst-cnt lst) 0)
    (setf (lst-vals lst) nil)))

(defun lst_filter (pred lst)
  (let ((newlist (loop
		   for i in (lst-vals lst) 
		   if (funcall pred i) 
		   collect i)))
  (progn
    (lst_reset lst)
    (lst_add_list newlist 
		 lst))))
