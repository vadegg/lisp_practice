(load "helpers.lisp")

(defun refalVariable (lst)
  (cond ((atom lst) nil)
        ((ne (smartLen lst) 2) nil)
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
        ((logItCond "aaaa"))
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

(defun takeFirtNElements (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) nil)
    ((cons (car lst) (takeFirtNElements (- n 1) (cdr lst))))
  )
)

(defun takeLstTail (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) lst)
    ((takeLstTail (- n 1) (cdr lst)))
  )
)

(defun putRefalVar (refalVar value)
  (cond ((null value) (put (car refalVar) (cadr refalVar) '\nil))
        ((put (car refalVar) (cadr refalVar) value))
  )
)

(defun smartLen (lst)
  (cond ((eq lst '\nil) 0)
        ((length lst))
  )
)

(defun smartMember (memb lst)
  (cond ((null lst) nil)
        ((or (smartEq memb (car lst)) (smartMember memb (cdr lst))))
  )
)

(defun cleanage_ (tmp prevElems)
  (cond 
    ((logItCond "cleanege_" tmp prevElems))
    ((null tmp))
        ((atom (car tmp)) (cleanage_ (cdr tmp) prevElems))
        ((and (refalVariable (car tmp))
              (not (smartMember (car tmp) prevElems))
         )
            (and (or (remprop (caar tmp) (cadar tmp)) t)
                                   (cleanage_ (cdr tmp) prevElems)
                              )
        )
   )
)

(defun cleanage (refalVar tmp prevElems)
  (and (logIt "cleanage:" refalVar tmp "except:" prevElems) 
       (or (remprop (car refalVar) (cadr refalVar)) t)
       (or (cleanage_ tmp prevElems) t)
  )
)

(defun elementNumberPrediction (n refalVar tmp lst previousElems)
  (let ((firstNElems (takeFirtNElements n lst)))
    (cond 
      
      ((not (= (smartLen firstNElems) n)) (and (cleanage refalVar tmp previousElems) nil))
      ((get (car refalVar) (cadr refalVar)) (and (Match_ tmp (takeLstTail n lst)) )
        or (putRefalVar refalVar firstNElems) t) 
         (cond
           ((Match_ tmp (takeLstTail n lst)))
           ((cleanage refalVar tmp previousElems) 
            (elementNumberPrediction (+ n 1) refalVar tmp lst 
                                    (cons refalVar previousElems)))
          )
      )
    )
  )
)

(defun eMatchRefalTemplate (refalVar tmp lst) 
  (let ((refalVarValue (get (car refalVar) (cadr refalVar))))
    (cond 
      (refalVarValue (let ((listLen (smartLen refalVarValue)) (firstListLenElems (takeFirtNElements (smartLen refalVarValue) lst)))
                       (cond
                         ((not (= listLen (smartLen firstListLenElems))) nil)
                         ((smartEq refalVarValue firstListLenElems) (Match_ tmp (takeLstTail (smartLen firstListLenElems) lst)))
                       )
                     )
      )
      ((elementNumberPrediction 0 refalVar tmp lst nil))
    )
  )
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
    ((or (null tmp) (atom (car tmp)) (ne (smartLen (car tmp)) 2)))

    ((eq (car (car tmp)) 's) (sMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'e) (eMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'v) (vMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'w) (wMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((fatalError "matchRefalTemplate"))
  )
)

