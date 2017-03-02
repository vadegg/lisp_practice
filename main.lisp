(defun put (a name p)
  (setf (get a name) p)
)

(defun toString (item)
  (string-trim "\"" (write-to-string item)) 
)


(defun smartEq (lst1 lst2)
  (cond
    ((and (null lst1) (null lst2)) t)
    ((or (null lst1) (null lst2)) nil)
    ((atom lst1) (eq lst1 lst2))
    ((and (smartEq (car lst1) (car lst2)) (smartEq (cdr lst1) (cdr lst2))))
  )
)

(defun smartNe (lst1 lst2)
  (not (smartEq lst1 lst2))
)

(defun concatAll_ (lst) 
  (cond
    ((null lst) "")
    ((atom (car lst)) 
        (concatenate 'string (toString (car lst)) " " (concatAll_ (cdr lst))))
    ((concatenate 'string "(" (concatAll_ (car lst)) ") " (concatAll_ (cdr lst))))
  )
)

(defun concatAll (&rest lst)
  (concatAll_ lst)
)

(defun cleanSymbol (sym) 
  (let ((symbolsList (SYMBOL-PLIST sym)))
    (cond ((null symbolsList))
          ((remprop sym (car symbolsList)) (cleanSymbol sym))
          ((fatalError "cleanSymbol"))
    )
  )
)

(defun cleanAllSymbols ()
  (and (cleanSymbol 's) 
       (cleanSymbol 'v) 
       (cleanSymbol 'w) 
       (cleanSymbol 'e)
  )
)

(defun ne (x y) 
  (not (eq x y))
)

(defun logIt (&rest lst)
  (logIt_ (concatAll_ lst))
)

(defun logAndReturn (&rest lst)
  (logAndReturn_ (car lst) (concatAll_ (cdr lst)))
)

(defun logIt_ (stringToLog) 
  (and (print (concatenate 'string "LOG: " stringToLog)) T)
)

(defun logAndReturn_ (returnValue stringToLog)
  (and (logIt stringToLog) returnValue)
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
  (let ((refalVarValue (get (car refalVar) (cadr refalVar))))
      (cond
        ((not (atom (car lst))) nil)

        ;if refalVarValue exists then:
        ;* continue matching with lst's tail 
        ;   if previous s's value equals to lst's first element
        ;* return nil else
        (refalVarValue (cond ((ne refalVarValue (car lst)) nil)
                             ((Match_ tmp (cdr lst)))
                       )
        )

        ;refalVarValue doesn't exist here and lst's first element is atom, 
        ;   remember it's value and continue
        ((and (put (car refalVar) (cadr refalVar) (car lst)) 
              (Match_ tmp (cdr lst))
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
  (let ((refalVarValue (get (car refalVar) (cadr refalVar))))
      (cond
        ;if refalVarValue exists then:
        ;* continue matching with lst's tail 
        ;   if previous s's value equals to lst's first element
        ;* return nil else
        (refalVarValue (cond ((smartNe refalVarValue (car lst)) nil)
                             ((Match_ tmp (cdr lst)))
                       )
        )

        ;refalVarValue doesn't exist here
        ;   remember it's value and continue
        ((and (put (car refalVar) (cadr refalVar) (car lst)) 
              (Match_ tmp (cdr lst))
        ))
      )
  )
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
    ((not (cleanAllSymbols)))
    ((let ((result (Match_ tmp lst)))
       (cond ((and (printResult) (cleanAllSymbols)) result)
             ((fatalError "Match"))
       )
    ))
  )
)

(defun Match_ (tmp lst)
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
          (eq (car tmp) (car lst))
     ) 
        (Match_ (cdr tmp) (cdr lst)))

    ;Refal variable in template
    ((refalVariable (car tmp)) (matchRefalTemplate tmp lst))

    ;First tmp's element is list
    ((and (not (atom (car tmp))) (atom (car lst))) nil)
        
    ;First template's and list's first elements are lists.
    ;Recursive continue
    ((and (Match_ (car tmp) (car lst)) (Match_ (cdr tmp) (cdr lst))))
  )
)

