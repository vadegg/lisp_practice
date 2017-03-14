(load "main.lisp")

(defun startTesting (lst)
  (cond ((null lst))
        ((and 
            (print "          !!!!!!!!!!!!!")
            (print (cons (car lst) (cadr lst)))
            (print "          !!!!!!!!!!!!!")
            (or (print (Match (car lst) (cadr lst))) t)
            (startTesting (cddr lst))
        ))
  )
)

(startTesting 
  '(
  ((e 1) (s a) (e 1) (s a)) (a b)

  ((w 1) (w 1)) ((m (b)) (m (b)))

  ((e 1) (e 1) (e 1) (s a)) (a a a a)

  ((e 1) (e 1) (e 1) (s a)) (a a a a a)

  ((e 1) (e 1) (e 1) (s a)) (a a a a a a)

  ((e 1) (e 1) (e 1) (s a)) (a a a a a a a)

  ((e 1) (e 1) (e 2) (e 1) (e 1)) (a a a a)

  )
)
