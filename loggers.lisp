(defun logIt (&rest lst)
  (logIt_ (concatAll_ lst))
)

(defun logAndReturn (&rest lst)
  (logAndReturn_ (car lst) (concatAll_ (cdr lst)))
)

(defun logIt_ (stringToLog) 
  (or (and (get 'SYSTEM 'LOG) (print (concatenate 'string "LOG: " stringToLog))) T)
)

(defun logAndReturn_ (returnValue stringToLog)
  (and (logIt (concatAll stringToLog)) returnValue)
)

(defun logItCond (&rest lst)
  (logAndReturn_ nil (concatAll_ lst))
)

(defun fatalError (stringToLog)
  (and (print (concatenate 'string "FATAL: " stringToLog)) T)
)

(defun printResult () 
  (and (print "RESULT:") 
       (printResult_ 's (SYMBOL-PLIST 's))
       (printResult_ 'w (SYMBOL-PLIST 'w))
       (printResult_ 'v (SYMBOL-PLIST 'v))
       (printResult_ 'e (SYMBOL-PLIST 'e))
  )
)

(defun printResult_ (varType lst)
  (cond ((null lst))
        ((and (print (concatAll
                        varType
                        (car lst)
                        " => "
                        (cadr lst)
                      )
               )
               (printResult_ varType (cddr lst))
        ))
  )
)


