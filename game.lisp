(load "structures.lisp")

(defun rg ()
  (load "game.lisp"))

(defun colors ()
  (loop for i from 1 to 40 do
	(format t "~d: ~c[~dmCOLORS~c[0m~%" i #\ESC i #\ESC)))

;; width of 70 works for home screen
(defparameter *width*  70)
(defparameter *height* (floor (* 2 (/ *width* 3))))
(defparameter *scale*  (floor (/ (+ *width* *height*) 2)))
(defparameter *world* (make-hash-table :test #'equal))

;; x y energy direction weapon bag
(defstruct player x y e d w b)

(defun create-player ()  
  (let ((x (random *width*))
	(y (random *height*))
	(e 100)
	(d 0)
	(w '())
	(b (list (cons 'COINS 0))))
    (setf (gethash (cons x y) *world*) 
	  (make-player :x x :y y :e e :d d :w w :b b))))

(defun add-point (x y p)
  (setf (gethash (cons x y) *world*) p))

(defun get-type (p)
  (gethash p *world*))

;; This will return nil if there is nothing there, 
;; otherwise it will return the type (p or s)
(defun value? (x y)
  (nth-value 0 (gethash (cons x y) *world*)))

(defun add-random-tree ()
  (add-point (random *width*) (random *height*) 'tree))

(defun add-random-rock ()
  (add-point (random *width*) (random *height*) 'rock))

(defun add-random-chest ()
  (add-point (random *width*) (random *height*) 'chest))

(defun show-world ()
  (loop for loc being each hash-key of *world*
	for typ being each hash-value of *world* do
	(format t "There is a ~S at ~d~%" typ loc)))

;; This will fill the world with an even number of rocks and trees;
;; work on making it more random.
(defun fill-world ()
  (progn
    (loop repeat (1+ (random *scale*)) do 
	  (progn (add-random-tree) 
		 (add-random-rock)))
    (loop repeat (random 3) do 
	  (progn 
	    (make-structure (random *width*) (random *height*) 'mountain)
	    (make-structure (random *width*) (random *height*) 'lake)))
    (loop repeat (random 5) do
	  (make-structure (random *width*) (random *height*) 'forest))
    (create-player)
    (add-random-chest)))

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

;; what this is going to have to do is
;; determine which way we will be
;; moving - so if we have a d
;; that is 0, we want to know what will
;; happen if we just add one to each x and y
(defun dir-factor (p)
  (let ((d (player-d p)))))


;; this is going to move in circles - we need to move based on
;; direction - maybe set keys (if (member x '(3 4 5)) 1 -1)
;; something like that - the if should check the next x or y
;; in the direction they are GOING, then change it if needed
(defun new-x (p)
  (let ((loc (cons (+ (player-x p) (dir-factor p)) 
		   (+ (player-y p) (dir-factor p)))))
    (if (gethash loc *world*)
      (progn 
	(setf (player-d p) (1+ (player-d p)))
	(new-x p))
      (setf (player-x p)
	    (mod (1+ (player-x p)) *width*)))))

(defun new-y (p)
  (let ((loc (cons (+ (player-x p) (dir-factor p)) 
		   (+ (player-y p) (dir-factor p)))))
    (if (gethash loc *world*)
      (progn
	(setf (player-d p) (1+ (player-d p)))
	(new-y p))
      (setf (player-y p)
	    (mod (1+ (player-y p)) *height*)))))

(defun move-player ()
  (let ((player (car (loop for v being each hash-value in *world*
		      if (equal 'player (type-of v)) 
		      collect v))))
    (progn
      (let ((old-loc (cons (player-x player) 
			   (player-y player))))
	(setf (player-x player) (new-x player))
	(setf (player-y player) (new-y player))
	;(setf (gethash (cons (player-x player) 
	;	     (player-y player)) *world*) player)
	;(remhash old-loc *world*)
	))
    ))

(defun get-purse (p)
  (assoc 'coins (player-b p)))

(defun add-coin (p)
  (setf (cdr (assoc 'coins (player-b p))) 
	(1+ (cdr (assoc 'coins (player-b p))))))

(defun set-weapon (p)
  (let ((choice (random 2)))
    (case choice
      (0 (progn
	   (setf (player-w p) 'Sword)
	   'sword))
      (1 (progn
	   (setf (player-w p) 'Bow)
	   'bow))
      (2 (progn
	   (setf (player-w p) 'Noodle)
	   'noodle)))))

;; what else can do inside. . .?
(defun open-chest (p)
  (let ((loot (random 5)))
    (case loot
      (1 (progn
	   (print "You found a coin!")
	   (add-coin p)))
      (2 (progn
	   (print "You found a snake!")
	   (setf (player-e p) (1- (player-e p)))))
      (3 (print ". . . empty. . . "))
      (4 (format t "~%A ~A" (set-weapon p)))
      (otherwise (print "A CURSE ON YOU!")))))

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
