(defun rgs ()
  "Reload Game Structures"
  (load "structures.lisp"))

;; These are the structure definitions: the things
;; that can exist in the world along with a way
;; to show them in the world.

(defparameter *mountain-shape* 	(list (cons  4 8 ) 
				      (cons  2 10) 
				      (cons  0 12) 
				      (cons  2 10) 
				      (cons  4 8 )))

(defparameter *lake-shape* 	(list (cons  2 4)
				      (cons  0 6) 
				      (cons  2 4)))

(defparameter *forest-shape* 	(list (cons  (random 2) (random 4))(cons  2 4)
				      (cons  0 (random 6))(cons  0 (random 6))
				      (cons  0 6)(cons -2 (random 4))
				      (cons -2 4)(cons  2 4)
				      (cons  2 (random 4))))

;; This will return nil if there is nothing there, 
;; otherwise it will return the type (p or s)
;; Used when looking for PLAYER in DRAW-WORLD
(defun value? (x y)
  (nth-value 0 (gethash (cons x y) *world*)))

(defun add-point (x y p)
  (setf (gethash (cons x y) *world*) p))

(defun get-shape (n)
  (case n
    ('mountain '*mountain-shape*)
    ('lake     '*lake-shape*)
    ('forest   '*forest-shape*)))

(defun make-structure (x y n)
    (loop for shape in (symbol-value (get-shape n)) do
	  (loop for index from (+ x (car shape)) 
		to (+ x (cdr shape)) do 
		(setf (gethash (cons index y) *world*) n))
	  (progn 
	    (setf y (1+ y))
	    (fresh-line))))

(defun show-struct-char (n)
  (case n
    ('rock     #\O)
    ('chest    #\=)
    ('player   (format nil "~c" #\f))
    ('door     #\D)
    ('plant    #\p)
    ('lake     #\w)
    ('tree     #\T)
    ('mountain #\M)
    (otherwise #\SPACE)))

(defun show-struct (n)
  (case n
    ('rock     (format t "~c[2mO~c[0m"  #\ESC #\ESC))
    ('chest    (format t "~c[31m=~c[0m" #\ESC #\ESC))
    ('player   (format t "~c[91mf~c[0m" #\ESC #\ESC))
    ('door     (format t "~c[31mD~c[0m" #\ESC #\ESC))
    ('plant    (format t "~c[31mp~c[0m" #\ESC #\ESC))
    ('lake     (format t "~c[~dmW~c[0m" #\ESC (nth (random 2)
						   (list 34 94)) #\ESC))
    ('tree     (format t "~c[~dmT~c[0m" #\ESC (nth (random 4)
						   (list 32 33 92 93)) #\ESC))
    ('mountain (format t "~c[~dmM~c[0m" #\ESC (nth (random 2)
						   (list 30 90)) #\ESC))
    (otherwise (format t "~c[0m~A~c[0m" #\ESC #\SPACE #\ESC))))

(defun add-random-item (name)
  (add-point (random *width*) (random *height*) name))

(defun find-mountains ()
  (loop for key being each hash-key in *world*
	for val being each hash-value in *world*
	if (equal 'mountain val) collect key))

(defun place-door ()
  (let ((mountains (find-mountains)))
    (let ((loc (nth (random (length mountains)) mountains)))
      (add-point (car loc) (cdr loc) 'door))))

(defun items-nearby ()
  (mapcar (lambda (x) (gethash x *world*)) (items-nearby-loc)))

(defun items-nearby-loc ()
  (mapcar (lambda (x) (get-next-loc x (get-player-loc)))
	  (loop for m in (movement-map)
		if (eq (cdr m) 0) collect (car m))))

(defun nearby-item-map ()
  (loop for name in (items-nearby)
	for loc in (items-nearby-loc)
	collect (cons name loc)))

(defun get-item-loc (item)
  (loop for val being each hash-value in *world*
       	     for key being each hash-key in *World*
	     if (equal item val) collect key))

(defun tornil ()
  (let ((truth (random 2)))
    (if (eq 1 truth)
      t
      nil)))
