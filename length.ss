; length of a list
(define length
	(lambda (ls)
		(if (null? ls)
			0
			(+ 1 (length(cdr ls))))))