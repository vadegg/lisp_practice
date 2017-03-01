(defun put (a name p)
  (setf (get a name) p)
)

(defun ne (x y) 
  (not (eq x y))
)

(defun recursiveConcat (lst)
  (cond ((null lst) "")
	((concatenate 'string (car lst) " " (recursiveConcat (cdr lst))))
  )
)

(defun logIt (&rest lst) 
  (and (print (recursiveConcat (cons "LOG: " lst))) T)
)

(defun logAndReturn (&rest lst)
  (and (logIt (cdr lst)) (car lst))
)

(defun fatalError (&rest lst)
  (logIt 'FATAL 'ERROR lst)
)

(defun refalVariable (lst)
  (cond ((ne (length lst) 2) nil)
        ((and (ne (car lst) 'e)
              (ne (car lst) 's)
              (ne (car lst) 'w)
              (ne (car lst) 'v)
         )
            nil
        )
        (t)
  )
)

(defun matchRefalTemplate (tmp lst) 
  (cond
    ;Error program structure checking
    ((or (null tmp) (null lst) (atom (car tmp)) (ne (length (car tmp)) 2))
     	(fatalError "matchRefalTemplate")
    )

    ((eq (car (car tmp)) 's) (sMatchRefalTemplate tmp lst))
    ((eq (car (car tmp)) 'e) (eMatchRefalTemplate tmp lst))
    ((eq (car (car tmp)) 'v) (vMatchRefalTemplate tmp lst))
    ((eq (car (car tmp)) 'w) (wMatchRefalTemplate tmp lst))
  )
)
(defun Match (tmp lst)
  (cond 
    	;Template and list are empty. End of parsing
    	((and (null tmp) (null lst)) (logAndReturn T 'Null 'Lists))

	;Either template or list are empty. Mathing fails
        ((or (null tmp) (null lst)) nil)

	;First template's element is atom and it is not equal to first list's element
        ((and (atom (car tmp)) (ne (car tmp) (car lst))) nil)

        ;Equal elements. Recursive continue
	((and (atom (car tmp)) 
              (eq (car tmp) (car lst))) 
                (Match (cdr tmp) (cdr lst)))

	;Refal variable in template
	((refalVariable (car tmp)) (logIt 'RefalVar))

	;First template's and list's first elements are lists. Recursive continue
	((and (Match (car tmp) (car lst)) (Match (cdr tmp) (cdr lst))))

	;Fatal error if no cond rule matched
        ((fatalError 'matchFunction))
  )
)

