(define (occurs-bound? v e)
	(cond 
		[(symbol? e) #f]
		[(eq? (car e) 'lambda)
			(or (occurs-bound? v (caddr e))
				(and (eq? v (caadr e)) 
					(occurs-free? v (caddr e))))]
		[else (or (occurs-bound? v (car e))
			(occurs-bound? v (cdr e)))]))

(define (occurs-free? v e)
	;in textbook
	#f)