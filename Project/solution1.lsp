(defun functionatom (x)
	(if (null x)
		nil
		(if (listp (car x))
			(append (functionatom (car x)) (functionatom (cdr x)))
			(cons (car x) (functionatom (cdr x))))))


(defun replaceword (symbol X)
	(if (null x)
		nil
		(if (listp (car x))
			(append (replaceword symbol (car x) (replaceword symbol (cdr x))))
			(if (equal symbol (car x))
				(cons (setf (car x) 'YYYY) (replaceword symbol (cdr x)))
				(cons (car x) (replaceword symbol (cdr x)))))))
