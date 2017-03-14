(load "helpers.lisp")
;функция-предикат, является ли аргумент рефал-переменной
;является, если список состоит из двух элементов, 
;   первый из которых - одна из букв e, s, w или v
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

;функция для сопоставления переменных типа s
(defun sMatchRefalTemplate (refalVar tmp lst) 
  (let ((refalVarValue (get (car refalVar) (cadr refalVar))))
      (cond
        ;логирование входа в функцию
        ((logItCond "smatch refalVar:" refalVar "tmp:" tmp "lst:" lst))
        
        ;отказ от сопоставления в случае, если первого элемента списка нет, или он не атом
        ((or (null lst) (not (atom (car lst)))) nil)

        ;если мы уже обрабатывали переменную с тем же именем и типом
        ;   проверить на равенство сохраненного значения переменной с первым элементом списка
        ;   продолжить сопоставление в случае равенства
        (refalVarValue (cond ((ne refalVarValue (car lst)) nil)
                             ((Match_ tmp (cdr lst)))
                       )
        )

        ;если мы не обрабатывали эту переменную ранее
        ;   сохранить ее новое значение
        ;   попробовать сопоставить
        ;   удалить значение, если не удалось сопоставить
        ((putMatchRemove refalVar tmp lst))
      )
  )
)

;функция запоминает текущее значение рефал-переменной
;пробует выполнить дальнейшее сопоставление
;удаляет значение рефал-переменной в случае неудачного сопоставления
(defun putMatchRemove (refalVar tmp lst)
        (or 
           (and (put (car refalVar) (cadr refalVar) (car lst)) nil)
           (Match_ tmp (cdr lst))
           (and (remprop (car refalVar) (cadr refalVar)) nil)
        )
)

;функция возвращает список из n первых элементов списка lst
(defun takeFirtNElements (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) nil)
    ((cons (car lst) (takeFirtNElements (- n 1) (cdr lst))))
  )
)

;функция возвращает список из последних (len lst)-n элементов списка lst
(defun takeLstTail (n lst)
  (cond 
    ((null lst) nil)
    ((= n 0) lst)
    ((takeLstTail (- n 1) (cdr lst)))
  )
)

;определение функции вставки значение рефал-переменной в копилку 
;   с учетом специального слова \nil
(defun putRefalVar (refalVar value)
  (cond ((null value) (put (car refalVar) (cadr refalVar) '\nil))
        ((put (car refalVar) (cadr refalVar) value))
  )
)

;определение функции длины списка с учетом специального слова \nil
(defun smartLen (lst)
  (cond ((eq lst '\nil) 0)
        ((length lst))
  )
)

;предсказание длины последовательности элеменов
(defun elementNumberPrediction (n refalVar tmp lst)
  (let ((firstNElems (takeFirtNElements n lst)))
    (cond
      ((logItCond "elementNuberPrediction n:" n "refalVar:" refalVar "tmp:" tmp "lst:" lst))
      
      ;сопоставление не удалось, если в списке осталось элементов меньше, чем мы хотим взять
      ((not (= n (smartLen firstNElems))) nil)
      
      ;если рефал-переменная уже обрабатывалась, пробуем сопоставить вместе с ней, либо взять последовательность большей длины
      ((get (car refalVar) (cadr refalVar)) (or (Match_ tmp (takeLstTail n lst)) (elementNumberPrediction (+ n 1) refalVar tmp lst))) 
      
      ;если рефал-переменная не обрабатывалась ранее, запоминаем ее значение, пробуем сопоставить, удаляем и берем бОльшую, если не удалось
      ((or 
         (and (putRefalVar refalVar firstNElems) nil)
         (Match_ tmp (takeLstTail n lst))
         (and (remprop (car refalVar) (cadr refalVar)) nil)
         (elementNumberPrediction (+ n 1) refalVar tmp lst)
      ))
    )
  )
)

;общая функция для обработки переменных типов e и v
(defun evMatchRefalTemplate (n refalVar tmp lst) 
  (let ((refalVarValue (get (car refalVar) (cadr refalVar))))
    (cond
      ;если эта рефал-переменная уже обрабатывалась, проверяем ее на равенство первым n элементам списка,
      ; если равны, продолжаем сопоставление
      (refalVarValue (let ((listLen (smartLen refalVarValue)) (firstListLenElems (takeFirtNElements (smartLen refalVarValue) lst)))
                       (cond
                         ((not (= listLen (smartLen firstListLenElems))) nil)
                         ((smartEq refalVarValue firstListLenElems) (Match_ tmp (takeLstTail (smartLen firstListLenElems) lst)))
                       )
                     )
      )

      ;если это новая рефал-переменная, начинаем предсказание длины последовательности
      ((elementNumberPrediction n refalVar tmp lst))
    )
  )
)

;выполнить подбор последовательности элементов, начиная с последовательности из одного элемента
(defun vMatchRefalTemplate (refalVar tmp lst) 
  (evMatchRefalTemplate 1 refalVar tmp lst)
)

;выполнить подбор последовательности элементов, начиная с пустой
(defun eMatchRefalTemplate (refalVar tmp lst) 
  (evMatchRefalTemplate 0 refalVar tmp lst)
)

(defun wMatchRefalTemplate (refalVar tmp lst) 
  (let ((refalVarValue (get (car refalVar) (cadr refalVar))))
      (cond
        ;если переменная уже существует, сравнить ее значение с первым элементом списка
        (refalVarValue (cond ((smartNe refalVarValue (car lst)) nil)
                             ((Match_ tmp (cdr lst)))
                       )
        )

        ;если не существует, запомнить его значение, попробовать сопоставить, удалить в случае неудачи
        ((putMatchRemove refalVar tmp lst
        ))
      )
  )
)

(defun matchRefalTemplate (tmp lst) 
  (cond
    ;отслеживаем ситуацию некорректного вызова функции из программы
    ((or (null tmp) (atom (car tmp)) (ne (smartLen (car tmp)) 2))
     (fatalError "matchRefalTemplate"))

    ;в зависимости от типа рефал-переменной отправляем в соответствующую функцию
    ((eq (car (car tmp)) 's) (sMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'e) (eMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'v) (vMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((eq (car (car tmp)) 'w) (wMatchRefalTemplate (car tmp) (cdr tmp) lst))
    ((fatalError "matchRefalTemplate"))
  )
)

