(define positives
	(lambda (li)
		(cond [(null? li) '()]
			[(positive? (car li)) (cons (car li) (positives (cdr li)))]
			[else (positives (cdr li))])))

(define filter-in 
	(lambda (pred? ls)
		(cond [(null? ls) '()]
			[(pred? (car ls))
				(cons (car ls)
					(filter-in pred? (cdr ls)))]
			[else (filter-in pred? (cdr ls))])))
(define sorted?
	(lambda (li)
		(or (< (length li) 2)
			(and (<= (car li) (cadr li))
				(sorted? (cdr li))))))