(define (lexical-address lexp)
	)


(lexical-address
	'((lambda (x y)
		(((lambda (z)
			(lambda (w y)
				(+ x z w y)))
		(list w x y z))
		(+ x y z)))
	(y z)))