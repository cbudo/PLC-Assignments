(define largest-in-lists
	(lambda (L)
		(let list-loop ([big-list L]
						[largest #f])
			(if (null? big-list)
				largest
				(let item-loop ([inner-list (car big-list)]
					[largest largest])
				(if (null? inner-list)
					(list-loop (cdr big-list) largest)
					(item-loop (cdr inner-list)
						(if (or (not largest)
								(> (car inner-list) largest))
							(car inner-list)
							largest))))))))

(define short-largest
	(lambda (L)
		(if (andmap null? L)
			#f
			(apply max (apply append L)))))

(define (list-index val ls)
	(let find ([lst ls] [counter 0])
		(if (null? lst)
			0
			(if (eq? (car lst) val)
				counter
				(find (cdr lst) (+ 1 counter))))))
