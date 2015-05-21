(ql:quickload "cl-charms")

(load "game.lisp")

(defpackage :game
  (:use :cl :cl-charms :game-driver)
  (:export :main :rgd))
(in-package :game)

(defun rgd ()
    "reload game: display"
    (load "display.lisp"))

(defun draw (typ)
  (charms:with-restored-cursor 
      charms:*standard-window*
	(charms:write-char-at-cursor
	    charms:*standard-window*
	      (if (char/= #\space (charms:char-at-cursor
	                              charms:*standard-window*))
    	    #\X
	        (game-driver::show-struct-char typ)))))

(defun main ()
  (charms:with-curses ()
  
    (charms:disable-echoing)
    
    (charms:enable-raw-input 
        :interpret-control-characters t)
        
    (charms:enable-non-blocking-mode 
        charms:*standard-window*)
    
    (loop named dr-loop
      with x = 0
      with y = 0
      for  c = (charms:get-char
              charms:*standard-window*
                    :ignore-error t)
      do (progn
      
          (charms:refresh-window
              charms:*standard-window*)
          
          (case c
            ((nil) nil)
            ((#\w) (decf y))
            ((#\a) (decf x))
            ((#\s) (incf y))
            ((#\d) (incf x))
            ((#\space) (draw 'player))
            ((#\q #\Q) (return-from dr-loop)))
      
          (multiple-value-bind (width height)
            (charms:window-dimensions charms:*standard-window*)
            (setf x (mod x width)
                  y (mod y height)))
              
          (charms:move-cursor charms:*standard-window* x y)))))
    
            
    ; (loop for y from 0 upto w do
    ; loop for x from 0 upto h do
    ;       (if (equal 'symbol (type-of (value? x y)))
    ;         (draw x y 'other)
    ;         (draw x y 'player)))))

;; there probably needs to be a better loop here
;; think about what is happening with this example, where
;; the loop continues forever, waiting for a char to be entered
;;
;; possibly rewrite a "draw" function that will send out a char to
;; the screen, then move the cursor up one space, then draw the next
;; for the size of the screen.  It would get the char from
;; the game.lisp show-struct-char function
;; could be like a loop inside another loop - almost as if 
;; the dr-loop below is sort of a game-loop, and on each iteration it 
;; would refresh the window
;; the "c" could set off the draw loop 
;; it would look similar to below, only the CASE would be setup different
;; and the x and y values wouldnt really matter much for the inside draw-loop

    ; (loop named dr-loop
    ;       with x = 0
    ;       with y = 0
    ;       for  c = (charms:get-char
    ;               charms:*standard-window*
    ;                     :ignore-error t)
    ;       do (progn
          
    ;           (charms:refresh-window
    ;               charms:*standard-window*)
              
    ;           (case c
    ;             ((nil) nil)
    ;             ((#\w) (decf y))
    ;             ((#\a) (decf x))
    ;             ((#\s) (incf y))
    ;             ((#\d) (incf x))
    ;             ((#\space) (draw))
    ;             ((#\q #\Q) (return-from dr-loop)))
          
    ;           (multiple-value-bind (width height)
    ;             (charms:window-dimensions charms:*standard-window*)
    ;             (setf x (mod x width)
    ;                   y (mod y height)))
                  
    ;           (charms:move-cursor charms:*standard-window* x y)))))