(define count-args
	(lambda L
		(length L)))

(define optional-args
	(lambda (x y . z)
		(list x y z)))

