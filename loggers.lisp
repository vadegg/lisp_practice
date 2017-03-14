
;преобразует в строки, конкатенирует и выводит на экран все свои аргументы
(defun logIt (&rest lst)
  (logIt_ (concatAll_ lst))
)

;аналогично logIt, только возвращает свой первый аргумент
(defun logAndReturn (&rest lst)
  (logAndReturn_ (car lst) (concatAll_ (cdr lst)))
)

;выводит на экран строку в зависимости от режима логирования 'SYSTEM-'LOG
(defun logIt_ (stringToLog) 
  (or (and (get 'SYSTEM 'LOG) (print (concatenate 'string "LOG: " stringToLog))) T)
)

;аналогично logIt_, только возвращает свой первый аргумент
(defun logAndReturn_ (returnValue stringToLog)
  (and (logIt (concatAll stringToLog)) returnValue)
)

;аналогично logIt, только возвращает nil вне зависимости от аргументов
;используется для отладки в ветках cond. не влияет на ход вычислений
(defun logItCond (&rest lst)
  (logAndReturn_ nil (concatAll_ lst))
)

;функция для вывода на экран сообщений о фатальной ошибке - 
;случаях, когда программа, вероятно, работает не так, как задумывал разработчик
(defun fatalError (stringToLog)
  (and (print (concatenate 'string "FATAL: " stringToLog)) T)
)

;функция выводит на экран содержимое копилки - значение рефаловских переменных
(defun printResult () 
  (and (print "RESULT:") 
       (printResult_ 's (SYMBOL-PLIST 's))
       (printResult_ 'w (SYMBOL-PLIST 'w))
       (printResult_ 'v (SYMBOL-PLIST 'v))
       (printResult_ 'e (SYMBOL-PLIST 'e))
  )
)

;функция выводит на экран значения списка свойств одного атома копилки
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


