(defun rg ()
  (load "game.lisp"))

(defparameter *width*  50)
(defparameter *height* 30)
(defparameter *scale*  50)
(defparameter *mountain-shape* (list (cons 4 8) (cons 2 10) (cons 0 12) (cons 2 10) (cons 4 8)))
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
    (setf (gethash (cons x y) *world*) (make-player :x x :y y :e e :d d :w w :b b))))


(defun add-point (x y p)
  (setf (gethash (cons x y) *world*) p))

(defun get-type (p)
  (gethash p *world*))

;; This will return nil if there is nothing there, 
;; otherwise it will return the type (p or s)
(defun yes-and-what-is-it? (x y)
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
    (loop repeat (1+ (random *scale*)) do (progn (add-random-tree) (add-random-rock)))
    (loop repeat (random 5) do (make-mountain (random *width*) (random *height*)))
    (create-player)
    (add-random-chest)))

(defun draw-world ()
  (loop for y from 0 upto *height* do
	(loop for x from 0 upto *width* do
	      (cond ((equal 'rock (yes-and-what-is-it? x y)) (princ #\4))
		    ((equal 'tree (yes-and-what-is-it? x y)) (princ #\0))
		    ((equal 'chest (yes-and-what-is-it? x y)) (princ #\=))
		    ((equal 'mountain (yes-and-what-is-it? x y)) (princ #\m))
		    ((equal 'player (type-of (yes-and-what-is-it? x y))) (princ #\f))
		    (t (princ #\space))))
	(fresh-line)))


(defun dir-factor (p)
  1)


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
      (mod (1+ (player-x p)) *width*))))

(defun new-y (p)
  (let ((loc (cons (+ (player-x p) (dir-factor p)) 
		   (+ (player-y p) (dir-factor p)))))
    (if (gethash loc *world*)
      (progn
	(setf (player-d p) (1+ (player-d p)))
	(new-y p))
      (mod (1+ (player-y p)) *height*))))

(defun move-player ()
  (let ((player (car (loop for v being each hash-value in *world*
		      if (equal 'player (type-of v)) collect v))))

    (progn
      (remhash (cons (player-x player) (player-y player)) *world*)
      (setf (player-x player) (new-x player))
      (setf (player-y player) (new-y player))
      (setf (gethash (cons (player-x player) (player-y player)) *world*) player))))

(defun get-purse (p)
  (assoc 'coins (player-b p)))

(defun add-coin (p)
  (setf (cdr (assoc 'coins (player-b p))) 
	(1+ (cdr (assoc 'coins (player-b p))))))

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
      (otherwise (print "A CURSE ON YOU!")))))

;; define a starting point and make a mountain shape around that
(defun make-mountain (x y)
    (loop for row-length in *mountain-shape* do
	  (loop for index from (+ x (car row-length)) to (+ x (cdr row-length)) do 
		(setf (gethash (cons index y) *world*) 'mountain))
	  (progn 
	    (setf y (1+ y))
	    (fresh-line))))

(defun game ()
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  ;;((do-action str)) do something - move, open chest, fight, etc
	  (t (move-player)
	     (draw-world)
	     (game)))))
  
	  
	


;;how to add mountains?  One M surrounded by little ones? mmMmm 
;;in all directions?
;;		mmmm
;;	      mmMMMMmm
;;	     mmMMMMMMmm
;;	      mmMMMMmm
;;	        mmmm
;;how about lakes?  same with O?
;;	    oo
;;	  ooOOoo
;;	oooOOOOooo
;;	  ooOOoo
;;	    oo
