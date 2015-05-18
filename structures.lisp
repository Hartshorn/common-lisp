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

(defparameter *forest-shape* 	(list (cons  2 4)(cons  2 4)
				      (cons  0 6)(cons  0 6)
				      (cons  0 6)(cons -2 4)
				      (cons -2 4)(cons  2 4)
				      (cons  2 4)))

(defun get-shape (n)
  (case n
    ('mountain 	'*mountain-shape*)
    ('lake 	'*lake-shape*)
    ('forest 	'*forest-shape*)))

(defun make-structure (x y n)
    (loop for shape in (symbol-value (get-shape n)) do
	  (loop for index from (+ x (car shape)) 
		to (+ x (cdr shape)) do 
		(setf (gethash (cons index y) *world*) n))
	  (progn 
	    (setf y (1+ y))
	    (fresh-line))))

(defun show-structure (n)
  (case n
    ('r (format t "~c[2m0~c[0m"   #\ESC #\ESC))
    ('t (format t "~c[3~dm4~c[0m" #\ESC (1+ (random 2)) #\ESC))
    ('c (format t "~c[35m=~c[0m"  #\ESC #\ESC))
    ('m (format t "~c[1mM~c[0m"   #\ESC #\ESC))
    ('s (format t "~c[30m~A~c[0m" #\ESC #\SPACE #\ESC))
    ('p (format t "~c[33mf~c[0m"  #\ESC #\ESC))
    ('l (format t "~c[34mM~c[0m"  #\ESC #\ESC))))


