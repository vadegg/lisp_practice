(defun put (a name p)
  (setf (get a name) p)
)

(defun ne (x y) 
  (not (eq x y))
)

(defun logIt (stringToLog) 
  (and (print (concatenate 'string "LOG: " stringToLog)) T)
)

(defun logAndReturn (returnValue stringToLog)
  (and (logIt stringToLog) returnValue)
)

(defun fatalError (stringToLog)
  (and (print (concatenate 'string "FATAL: " stringToLog)) T)
)

(defun refalVariable (lst)
  (cond ((atom lst) nil)
        ((ne (length lst) 2) nil)
        ((or  (eq (car lst) 'e)
              (eq (car lst) 's)
              (eq (car lst) 'w)
              (eq (car lst) 'v)
        ))

  )
)

(defun sMatchRefalTemplate (refalVar tmp lst) 
  (let ((refalVarValue (get (car refalVar) (cdr refalVar))))
      (cond 
        ;if refalVar exists then return:
        ;* null if it's value is not equal to first list's element 
        ;* recursive Match calling else
        (refalVarValue (cond ((eq refalVariable (car refalVar))
                                (Match tmp (cdr lst))
                             )
                       )
        )
        ((not (atom (car lst))) nil)
        ((and (put (car refalVar) (cadr refalVar) (car lst)) 
              (Match tmp (cdr lst))
        ))
      )
  )
)

(defun eMatchRefalTemplate (refalVar tmp lst) 
  (logIt "eMatchRefalTemplate")
)

(defun vMatchRefalTemplate (refalVar tmp lst) 
  (logIt "vMatchRefalTemplate")
)

(defun wMatchRefalTemplate (refalVar tmp lst) 
  (logIt "wMatchRefalTemplate")
)

(defun matchRefalTemplate (tmp lst) 
  (cond
    ;Error program structure checking
    ((or (null tmp) (null lst) (atom (car tmp)) (ne (length (car tmp)) 2))
         (fatalError "matchRefalTemplate")
    )

    ((eq (car (car tmp)) 's) (sMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'e) (eMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'v) (vMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'w) (wMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((fatalError "matchRefalTemplate"))
  )
)
(defun Match (tmp lst)
  (cond 
        ;Template and list are empty. End of parsing
        ((and (null tmp) (null lst)) T)

        ;Either template or list are empty. Mathing fails
        ((or (null tmp) (null lst)) nil)

        ;First template's element is atom and it
        ;is not equal to first list's element
        ((and (atom (car tmp)) (ne (car tmp) (car lst))) nil)

        ;Equal elements. Recursive continue
        ((and (atom (car tmp)) 
              (eq (car tmp) (car lst))) 
                (Match (cdr tmp) (cdr lst)))

        ;Refal variable in template
        ((refalVariable (car tmp)) (matchRefalTemplate tmp lst))

        ;First tmp's element is list
        ((and (not (atom (car tmp))) (atom (car lst))) nil)
        
        ;First template's and list's first elements are lists.
        ;Recursive continue
        ((and (Match (car tmp) (car lst)) (Match (cdr tmp) (cdr lst))))

        ;Fatal error if no cond rule matched
        ((fatalError "matchFunction"))
  )
)

