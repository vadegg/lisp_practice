(load "refalMatching.lisp")
(load "helpers.lisp")
(load "loggers.lisp")

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
((and (printResult) (logItCond "MatchFunc:" tmp "with:" lst)))
    ;Template and list are empty. End of parsing
    ((and (null tmp) (null lst)) T)

    ;Either template or list are empty. Mathing fails
    ((null tmp) nil)

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

