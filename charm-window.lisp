(defpackage charms-test
  (:use :cl :cl-charms)
  (:export :main))
(in-package :charms-test)

(defun draw ()
  (charms:with-restored-cursor 
      charms:*standard-window*
	(charms:write-char-at-cursor
	    charms:*standard-window*
	      (if (char/= #\SPACE (charms:char-at-cursor 
	                              charms:*standard-window*))
	   	  #\SPACE 
		    #\*))))

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
                ((#\space) (draw))
                ((#\q #\Q) (return-from dr-loop)))
          
              (multiple-value-bind (width height)
                (charms:window-dimensions charms:*standard-window*)
                (setf x (mod x width)
                      y (mod y height)))
                  
              (charms:move-cursor charms:*standard-window* x y)))))

	
