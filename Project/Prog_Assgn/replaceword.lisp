(defun replaceword (symbol X)
	(if (null x)
		nil
		(if (listp (car x))
			(cons (replaceword symbol (car x)) (replaceword symbol (cdr x)))
			(if (equal symbol (car x))
				(cons (setf (car x) 'YYYY) (replaceword symbol (cdr x)))
				(cons (car x) (replaceword symbol (cdr x)))))))