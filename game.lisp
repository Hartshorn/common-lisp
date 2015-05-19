(load "structures.lisp")
(load "player.lisp")

(defun rg ()
  "Reload Game"
  (load "game.lisp"))


(defun colors ()
  (loop for i from 90 to 107 do
	(format t "~d: ~c[~dmCOLORS~c[0m~%" i #\ESC i #\ESC))
	(loop for i from 30 to 47 do
	(format t "~d: ~c[~dmCOLORS~c[0m~%" i #\ESC i #\ESC)))

;; width of 70 works for home screen
(defparameter *width*  50)
(defparameter *height* (floor (* 2 (/ *width* 3))))
(defparameter *scale*  (floor (/ (+ *width* *height*) 2)))
(defparameter *world* (make-hash-table :test #'equal))

(defun show-world ()
  (loop for loc being each hash-key of *world*
	for typ being each hash-value of *world* do
	(format t "There is a ~S at ~d~%" typ loc)))

(defun fill-world ()
  (progn
    (loop repeat (1+ (random *scale*)) do 
	  (progn (add-random-item 'tree) 
		 (add-random-item 'rock)))
    (loop repeat (1+ (random 5)) do 
	  (progn 
	    (make-structure (random *width*) (random *height*) 'mountain)
	    (make-structure (random *width*) (random *height*) 'lake)))
    (loop repeat (1+ (random 5)) do
	  (make-structure (random *width*) (random *height*) 'forest))
    (add-random-item 'chest)
    (place-player)))

(defun draw-world ()
  (loop for y from 0 upto *height* do
	(loop for x from 0 upto *width* do
	      (cond ((equal 'rock     (value? x y)) (show-structure 'r))
		    ((equal 'tree     (value? x y)) (show-structure 't))
		    ((equal 'chest    (value? x y)) (show-structure 'c))
		    ((equal 'mountain (value? x y)) (show-structure 'm))
		    ((equal 'lake     (value? x y)) (show-structure 'l))
		    ((equal 'forest   (value? x y)) (show-structure 't))
		    ((equal 'player (type-of (value? x y))) 
		     				    (show-structure 'p)) 
		    (t (show-structure 's)))) 
	(fresh-line)))

(defun game ()
  (fresh-line)
  (draw-world)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  ;;((do-action str)) do something - move, open chest, fight, etc
	  (t 
	     (move-player)
	     (draw-world)
	     (game)))))
