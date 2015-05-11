(defun rp ()
  (load "particle.lisp"))

(defparameter *time* 0)
(defparameter *world* '())
(defparameter *width* 20)
(defparameter *height* 10)
(defparameter *max-v* 1000)
(defparameter *max-a* 100)
(defparameter *d-list* (list (cons 5 3) (cons 3 5) (cons 2 6) (cons 6 2) (cons 0 4) 
			     (cons 4 0) (cons 7 5) (cons 5 7) (cons 1 3) (cons 3 1)))
(defparameter *c-list* (list (cons 1 5) (cons 5 1) (cons 3 7) (cons 7 3)))

(defstruct p x y d)

(defun p_func ()
  (make-p :x (floor (/ *width* 2)) :y (floor (/ *height* 2)) :d 0))

(defun fx ()
  (mod (* 3 *time*) *width*))

(defun fy ()
  (mod (* 1 *time*) *height*))

(defun fd ()
  (mod (+ 1 *time*) 8))

(defun p_random ()
  (make-p :x (random *width*) :y (random *height*) :d (random 8)))

(defun p_random_three ()
  (loop repeat 3 do (push (p_random) *world*)))


(defun p_move (p)
  (let ((x (p-x p))
	(y (p-y p))
	(d (p-d p)))
    (if (p_in_corner p) 
      (setf (p-d p) (p_turn_corner d))
      (if (p_on_wall p) 
	(setf (p-d p) (p_flip_d d))))
    (setq d (p-d p))
    (setf (p-x p) (+ x (cond ((and (> d 0) (< d 4)) 1)
			     ((or  (= d 0) (= d 4)) 0)
			     ( t -1))))
    (setf (p-y p) (+ y (cond ((and (> d 2) (< d 6)) -1)
			     ((or  (= d 6) (= d 2)) 0)
			     ( t 1))))))

(defun p_move_func (p)
	(let ((d (p-d p)))
	  (if (p_in_corner p) 
	    (setf (p-d p) (p_turn_corner d))
	    (if (p_on_wall p) 
	      (setf (p-d p) (p_flip_d d))))
	  (setq d (p-d p))
	  (setf (p-x p) (fx))
	  (setf (p-y p) (fy))))



(defun p_flip_d (d)
  (setq d (cdr (assoc d *d-list*))))

(defun p_turn_corner (d)
  (setq d (cdr (assoc d *c-list*))))

(defun p_in_corner (p)
  (let ((loc (cons (p-x p) (p-y p))))
    (cond (((cornerp) t) 
	    (t nil)))))

(defun p_on_wall (p)
  (let ((x (p-x p))
	(y (p-y p)))
    (if (and (not_corner) (or (eq x *width*) (<= x 0) (eq y *height*) (<= y 0)) t))))
	  

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

(defun p_time ()
  (mapcar #'p_move *world*)
  (p_world)
  (setq time (1+ *time*)))

(defun particle ()
  (p_world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (mapcar #'p_move_func *world*)
	     (setq *time* (1+ *time*))
	     (particle)))))
