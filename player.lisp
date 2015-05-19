(defun rgp ()
  "Reload Game Player"
  (load "player.lisp"))

;; x y energy direction weapon bag
(defstruct player x y e d w b)

(defun create-player ()  
  (let ((x (random *width*))
	(y (random *height*))
	(e 100)
	(d (random 8))
	(w '())
	(b (list (cons 'COINS 0))))
    (setf (gethash (cons x y) *world*) 
	  (make-player :x x :y y :e e :d d :w w :b b))))

;; change the direction - set with 1 or -1 passed in.
(defun turn-player (n)
  (setf (player-d (get-player)) (mod (1+ (player-d (get-player))) 8)))

(defun get-player ()
  (car 
    (loop for v 
	  being each hash-value in *world* 
	  if (equal 'player (type-of v)) 
	  collect v)))

(defun get-player-loc ()
  (cons (player-x (get-player)) 
	(player-y (get-player))))

;; What is the next position in this direction?
(defun get-next-loc (dir loc)
  (let ((new-x 0)
	(new-y 0))
    (progn
      (cond
	((and (> dir 0) (<= dir 3)) (setf new-x  1))
	((and (> dir 4) (<= dir 7)) (setf new-x -1)))
      (cond
	((and (< dir 6) (>= dir 3)) (setf new-y -1))
	((or (eq dir 7) (eq dir 0) (eq dir 1)) (setf new-y 1)))
      (setf (car loc) (mod (+ (car loc) new-x) *width*))
      (setf (cdr loc) (mod (+ (cdr loc) new-y) *height*))))
  loc)

;; Look for a structure in the location one space away
(defun can-move-p (dir loc)
  (let ((new-loc (get-next-loc dir loc)))
    (if (gethash new-loc *world*) 0 1)))

;; This is a map of possible moves: 1 yes 0 no
;; paired with the directions possible.  (dir . yes/no)
;; '((4 . 0) (5 . 1) (6 . 0) (7 . 1) (0 . 1) (1 . 1) (2 . 0) (3 . 1))
;; the order changes depending on the starting direction, but
;; the first direction will always be the last in the list
;; (the first one pushed on)
(defun movement-map ()
  (let ((dir (player-d (get-player))))
    (let ((directions (loop repeat 8 do
			    (setf dir (mod (1+ dir) 8)) 
			    collect dir)))
      (loop for d in directions
	    collect (cons d (can-move-p d (get-player-loc)))))))

(defun chest-nearby ()
  (if (member 'chest
    (mapcar (lambda (x) (gethash x *world*))
	  (mapcar (lambda (x) (get-next-loc x (get-player-loc)))
		  (loop for m in (movement-map)
			if (eq (cdr m) 0) collect (car m)))))
	  t
	  nil))

(defun place-player ()
  (create-player)
  (if (eq 0 (apply #'+ (mapcar #'cdr (movement-map))))
    (progn
      (remhash (cons (player-x (get-player))
		     (player-y (get-player)))
	       *world*)
      (place-player))
    'World-Filled!))

(defun set-direction ()
  (if (eq 0 (cdr (assoc (player-d (get-player)) (movement-map))))
    (let ((factor (random 2)))
      (if (eq 0 factor)
	      (turn-player  1)
	      (turn-player -1))
      (set-direction))))

(defun move-player ()
  (set-direction)
  (let ((loc (get-next-loc (player-d (get-player)) (get-player-loc)))) 
    (progn
      (setf p (copy-player (get-player)))
      (setf (player-x p) (car loc))
      (setf (player-y p) (cdr loc))
      (setf (player-e p) (1- (player-e p)))
      (remhash (get-player-loc) *world*)
      (setf (gethash (cons (player-x p)
			   (player-y p)) *world*) p))))

(defun get-chest-loc ()
  (car (loop for val being each hash-value in *world*
       	     for key being each hash-key in *World*
	     if (equal 'chest val) collect key)))

;; what else can we do inside. . .?
(defun open-chest ()
  (let ((loot (random 5)))
    (case loot
      (1 (progn
	   (print "You found a coin!")
	   (add-coin (get-player))))
      (2 (progn
	   (print "You found a snake!")
	   (setf (player-e (get-player)) (1- (player-e (get-player))))))
      (3 (print ". . . empty. . . "))
      (4 (format t "~%A ~A" (set-weapon (get-player))))
      (otherwise (print "A CURSE ON YOU!"))))
  (remhash (get-chest-loc) *world*))

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
