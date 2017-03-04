(defun put (a name p)
  (setf (get a name) p)
)

(defun toString (item)
  (string-trim "\"" (write-to-string item)) 
)


(defun smartEq (lst1 lst2)
  (cond
    ((or (and (eq lst1 '\nil) (null lst2)) 
         (and (eq lst2 '\nil) (null lst1)))
    )
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

