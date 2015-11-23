(defclass person ()
    ((firstname
        :initarg :fn
        :accessor firstname)
     (lastname
        :initarg :ln
        :accessor lastname)
     (age
         :initarg :age
         :accessor age)))

(defclass child (person)
    ((parent
        :initarg :p
        :accessor parent)))

(defgeneric printp (person))
(defgeneric printp (child))
(defgeneric newp (fn ln age))
(defgeneric newc (fn ln age p))

(defmethod printp (person)
    (format t "Firstname: ~S~%Lastname: ~S~%Age: ~D~%"
        (firstname person)
        (lastname person)
        (age person)))

(defmethod printc (child)
    (progn
        (format t "Firstname: ~S~%Lastname: ~S~%Age: ~D~%Parent:~%"
            (firstname child)
            (lastname child)
            (age child))
        (printp (parent child))))

(defmethod newp (fn ln age)
        (make-instance 'person :fn fn :ln ln :age age))

(defmethod newp :around (fn ln age)
    (print "Creating new Person . . .")
    (let ((new-person (call-next-method)))
        (printp new-person)
    new-person))

(defmethod newc (fn ln age p)
        (make-instance 'child :fn fn :ln ln :age age :p p))

(defmethod newc :around (fn ln age p)
    (print "Creating new Child. . .")
    (let ((new-child (call-next-method)))
        (printc new-child)
    new-child))
