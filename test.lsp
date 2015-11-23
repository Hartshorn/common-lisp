(defgeneric f (x y))

(defmethod f ((x integer) y ) 1)


(print (f 1 2.0))


(defmethod f ((x integer) (y real)) 2)

(print (f 1 2.0))

(print (f 1 '(a b c)))

(defun range-helper (x)
    (if (= x 0)
        (list x)
        (cons x (range-helper (- x 1)))))

(defun range (x)
    (reverse (range-helper (- x 1))))


(defmacro doit (exp for var in list cond cond-test)

    (let ((result (gensym)))
    `(let ((,result nil))

        (loop for ,var in ,list
            ,cond ,cond-test

            do (setq ,result (append ,result (list ,exp))))
            ,result)))

(print (macroexpand-1 '(doit x for x in (range 10) if (= (mod x 2) 0))))
(print (doit x for x in (range 10) if (= (mod x 2) 0)))

(let ((x 10)) (print (incf x 10)))
