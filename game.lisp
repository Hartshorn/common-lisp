(defpackage :game-driver
	(:use :cl)
	(:export :show-struct-char))
	
(in-package :game-driver)

(load "structures.lisp")
(load "player.lisp")

(defun rg ()
  "Reload Game"
  (load "game.lisp"))

;; The compass:
;;          |
;;        5 4 3
;;			--6 f 2--
;;        7 0 1
;;          |

(defun colors ()
  (loop for i from 90 to 107 do
	(format t "~d: ~c[~dmCOLORS~c[0m~%" i #\ESC i #\ESC))
  (loop for i from 30 to 47 do
	(format t "~d: ~c[~dmCOLORS~c[0m~%" i #\ESC i #\ESC)))

;; width of 70 works for home screen
(defparameter *width*  70)
(defparameter *height* (floor (* 2 (/ *width* 3))))
(defparameter *scale*  (floor (/ (+ *width* *height*) 2)))
(defparameter *world*  (make-hash-table :test #'equal))
(defparameter *plant-energy* 80)

(defun show-world ()
  (loop for loc being each hash-key of *world*
	for typ being each hash-value of *world* do
	(format t "There is a ~S at ~d~%" typ loc)))

(defun fill-world ()
  (progn
    (loop repeat (1+ (random *scale*)) do 
	  (progn (add-random-item 'tree) 
		 (add-random-item 'rock)
		 (add-random-item 'plant)))
    (loop repeat (1+ (random 5)) do 
	  (progn 
	    (make-structure (random *width*) (random *height*) 'mountain)
	    (make-structure (random *width*) (random *height*) 'lake)))
    (loop repeat (1+ (random 5)) do
	  (make-structure (random *width*) (random *height*) 'forest))
    (add-random-item 'chest)
    (place-player)
    (place-door))
  'done)

(defun draw-world ()
  (loop for y from 0 upto *height* do
	(loop for x from 0 upto *width* do
	     (if (equal 'symbol (type-of (value? x y)))
	       (show-struct (value? x y))
	       (show-struct (type-of (value? x y)))))
	(fresh-line)))

(defun do-action (str)
  (case str
    ('turnl  (progn (turn-player  1) (do-action 'player)))
    ('turnr  (progn (turn-player -1) (do-action 'player)))
    ('player (print (get-player)))
    ('show   (print (show-world)))
    ('plant  (if (member 'plant (items-nearby))
	       (print (eat-plant))
	       (print "No Plant Here!")))
    ('chest  (if (member 'chest (items-nearby))
	       (print (open-chest))
	       (print "No Chest Here!")))))

(defun game ()
  (fresh-line)
  (let ((str (read-line))         )
    (cond ((equal str "quit"      ) ())
	  ((equal str "turn left" ) (do-action 'turnl ) (game))
	  ((equal str "turn right") (do-action 'turnr ) (game))
	  ((equal str "player"    ) (do-action 'player) (game))
	  ((equal str "show"      ) (do-action 'show  ) (game))
	  ((equal str "open chest") (do-action 'chest ) (game))
	  ((equal str "eat plant" ) (do-action 'plant ) (game))
	  (t (move-player)
	     (draw-world)
	     (game)))))
