(defun rp ()
  (load "particle.lisp"))

(defparameter *world* '())
(defparameter *width* 100)
(defparameter *height* 30)

(defparameter *c-flip* (list (cons 1 5) (cons 5 1) 
														 (cons 3 7) (cons 7 3)))

(defparameter *corners* (list (cons 0 0) (cons *width* *height*) 
															(cons *width* 0) (cons 0 *height*)))

(defparameter *opposites* (list (cons 0 4)
																(cons 1 5)
																(cons 2 6)
																(cons 3 7)
																(cons 4 0)
																(cons 5 1)
																(cons 6 2)
																(cons 7 3)))

(defstruct p x y d)

(defun find-nils ()
  (loop for i in *world* if (not (p-d i)) collect i))

(defun get-pos (p)
  (cons (p-x p) (p-y p)))

(defun get-member-direction (loc)
  (let ((x (car loc))
	(y (cdr loc)))
    (loop for p in *world*
	  if (and (eq (p-x p) x)
		  (eq (p-y p) y))
	  return (p-d p))))


(defun add-one (x y d)
  (push (make-p :x x :y y :d d) *world*))

(defun p_random ()
  (make-p :x (random *width*) :y (random *height*) :d (random 8)))

(defun p_random_three ()
  (loop repeat 3 do (push (p_random) *world*)))

(defun make-two-near (loc d)
  (let ((x (car loc))
	(y (cdr loc)))
    (push (make-p :x (1+ x) :y (1- y) :d (mod (1+ d) 8)) *world*)
    (push (make-p :x (1- x) :y (1+ y) :d (mod (1- d) 8)) *world*)))

(defun p_move (p)
  (check-outliers)
  (let ((x (p-x p))
	(y (p-y p))
	(d (set-d p)))
    (setf (p-d p) d)
    (setf (p-x p) (+ x (cond ((and (> d 0) (< d 4)) 1)
			     ((or  (= d 0) (= d 4)) 0)
			     ( t -1))))
    (setf (p-y p) (+ y (cond ((and (> d 2) (< d 6)) -1)
			     ((or  (= d 6) (= d 2)) 0)
			     ( t 1))))))

(defun check-outliers ()
  (loop for i in *world* 
	if (eq (p-d i) nil) do (setf (p-d i) (random 8))
	(if (or (< (p-x i) 0)
		(< (p-y i) 0)
		(> (p-x i) *width*)
		(> (p-y i) *height*))
	do
	(setf (p-d i) (mod (+ (p-d i) 2) 8)))))


(defun set-d (p)
  (let ((loc (cons (p-x p) (p-y p)))
	(d (p-d p)))
    (cond ((p-collision loc d)
	   (random 8))
	  ((and (in-corner loc)
		(d-in-corner loc d))
	   (cdr (assoc d *c-flip*)))
	  ((and (on-wall loc)
		(d-in-wall loc d))
	   (flip-d loc d))
	  (t d))))

(defun p-collision (loc d)
  (let ((directions (mapcar (lambda (x) (cdr (assoc x *opposites*))) 
			    (mapcar #'get-member-direction 
				    (intersection (make-position-list loc) 
						  (mapcar #'get-pos *world*) :test 'equal)))))
    (member d directions)))


(defun make-position-list (loc)
  (let ((plus-x (1+ (car loc)))	(minus-x (1- (car loc)))
	(plus-y (1+ (cdr loc))) (minus-y (1- (cdr loc))))
    (list (cons (car loc) minus-y)
	  (cons minus-x minus-y)
	  (cons minus-x (cdr loc))
	  (cons minus-x plus-y)
	  (cons (car loc) plus-y)
	  (cons plus-x plus-y)
	  (cons plus-x (cdr loc))
	  (cons plus-x minus-y))))

(defun car-is-width (loc)
  (eq (car loc) *width*))

(defun car-is-zero (loc)
  (eq (car loc) 0))

(defun cdr-is-height (loc)
  (eq (cdr loc) *height*))

(defun cdr-is-zero (loc)
  (eq (cdr loc) 0))

(defun flip-d (loc d)
  (cond ((car-is-width loc)
	 (case d
	   ((2) 6)
	   ((3) 5)
	   ((1) 7)
	   ((0) 4)
	   ((4) 0)))
	((car-is-zero loc)
	 (case d
	   ((5) 3)
	   ((6) 2)
	   ((7) 1)
	   ((4) 0)
	   ((0) 4)))
	((cdr-is-height loc)
	 (case d
	   ((7) 5)
	   ((1) 3)
	   ((0) 4)
	   ((6) 2)
	   ((2) 6)))
	((cdr-is-zero loc)
	 (case d 
	   ((3) 1)
	   ((4) 0)
	   ((5) 7)
	   ((6) 2)
	   ((2) 6)))
	((t d))))

(defun in-corner (loc)
  (loop for i in *corners* if (equal i loc) return t))

(defun d-in-corner (loc d)
  (cond ((and (equal loc (cons 0 0))
	      (eq d 5))
	 t)
	((and (equal loc (cons *width* 0))

	      (eq d 3))
	 t)
	((and (equal loc (cons 0 *height*))
	      (eq d 7))
	 t)
	((and (equal loc (cons *width* *height*))
	      (eq d 1))
	 t)))

(defun on-wall (loc)
  (cond ((or (eq 0 (car loc))
	     (eq *width* (car loc))) 
	 t)
	((or (eq 0 (cdr loc))
	     (eq *height* (cdr loc)))
	 t)))

(defun d-in-wall (loc d)
  (cond ((and (equal (car loc) *width*)
	      (member d '(1 2 3)))
	 t)
	((and (equal (car loc) 0)
	      (member d '(5 6 7)))
	 t)
	((and (equal (cdr loc) *height*)
	      (member d '(0 1 7)))
	 t)
	((and (equal (cdr loc) 0)
	      (member d '(3 4 5)))
	 t)))


(defun p_world ()
  (loop 
    initially (loop for i below (+ 3 *width*) do (princ "_" )) 
	for y below (1+ *height*)
	do (progn (fresh-line)
		  (princ "|")
		  (loop for x below (1+ *width*)
			do (princ (cond ((some (lambda (p)
						 (and (= (p-x p) x)
						      (= (p-y p) y)))
					       *world*) #\*)
					(t #\space))))
		  (princ "|")
		  (fresh-line))
	finally (loop for i below (+ 3 *width*) do (princ "=" ))))

(defun particle ()
  (p_world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (mapcar #'p_move *world*)
	     (particle)))))


(defun debug-particle ()
  (p_world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (mapcar #'p_move *world*)
	     *world*
	     (particle)))))

