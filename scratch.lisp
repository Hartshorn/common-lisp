(setf c (with-output-to-string (stream) 
    (extensions:run-program "clear" nil :output stream)))