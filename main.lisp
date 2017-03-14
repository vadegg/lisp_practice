(load "refalMatching.lisp")
(load "helpers.lisp")
(load "loggers.lisp")

;установка режима - 
;t для вывода на экран всей отладочной информации
;nil для режима "релиза"
(put 'SYSTEM 'LOG nil)

;главная функция
;чистит копилку для корректной работы
;затем вызывает функцию непосредственной обработки ввода
;и снова чистит копилку
(defun Match (tmp lst)
  (cond 
    ((not (cleanAllSymbols)))
    ((let ((result (Match_ tmp lst)))
       (cond ((and (print "============") 
                   (printResult) 
                   (cleanAllSymbols)
              ) result)
             ((fatalError "Match"))
       )
    ))
  )
)

;функция обработки шаблона и списка
(defun Match_ (tmp lst)
  (cond
    ;логирует состояние копилки при каждом вызове функции
    ((and (get 'SYSTEM 'LOG) (printResult) (logItCond "MatchFunc:" tmp "with:" lst)))

    ;сопоставление прошло успешно, если на входе имеем два пустых списка
    ((and (null tmp) (null lst)) T)

    ;сопоставление прошло не успешно, если список с шаблоном пуст, а список для сопоставления не пуст
    ;ситуация, когда шаблон не пуст, а список пуст возможна, например, '((e 1)) 'nil - должно сопоставиться
    ((null tmp) nil)

    ;если первый элемент шаблона - атом, просто сравниваем его с первым элементом списка
    ((and (atom (car tmp)) (ne (car tmp) (car lst))) nil)

    ((and (atom (car tmp)) 
          (eq (car tmp) (car lst))
     ) 
        (Match_ (cdr tmp) (cdr lst)))

    ;если первый элемент шаблона - рефаловская переменная, отправляемся в функцию обработки рефал-переменных
    ((refalVariable (car tmp)) (matchRefalTemplate tmp lst))

    ;отслеживаем ситуацию, когда первый элемент шаблона список(не рефал-переменная), а первый элемент списка - атом
    ((and (not (atom (car tmp))) (atom (car lst))) nil)
        
    ;оба первых элемента шаблона и списка являются списками - рекурсивно обрабатываем
    ((and (Match_ (car tmp) (car lst)) (Match_ (cdr tmp) (cdr lst))))
  )
)

