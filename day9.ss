(define (flatten slist)
	(let flatten ([s1 slist])
		(cond [(null? s1) '()]
			[(symbol? (car s1))
				(cons (car s1) 
					(flatten (cdr s1)))]
			[else
				(append (flatten (car s1)) (flatten (cdr s1)))])))

(define (notate-depth slist)
	(let notate ([s1 slist] [depth 0])
		(cond [(null? s1) '()]
			[(symbol? (car s1))
				(cons (list (car s1) depth)
					(notate (cdr s1) depth))]
			[else
				(cons (notate (car s1) (+ depth 1)) (notate (cdr s1) depth))])))

