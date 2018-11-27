(defun maxdepth(x)
	(if (null x)
		0
		(get_depth x 1)
	)
)

(defun get_depth (x depth)
	(if (null x)
		depth
		(if (listp (car x))
			(max (get_depth (car x) (+ depth 1))
				(get_depth (cdr x) depth)
			)
			(get_depth (cdr x) depth)
		)
	)
)