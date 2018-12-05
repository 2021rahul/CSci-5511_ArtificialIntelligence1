(defun concatlists (list1 list2)
	(if (null list1)
		list2
		(cons (car list1) (concatlists (cdr list1) list2))))

(defun functionatom (x)
	(if (null x)
		nil
		(if (listp (car x))
			(concatlists (functionatom (car x)) (functionatom (cdr x)))
			(cons (car x) (functionatom (cdr x))))))
