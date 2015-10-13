(define (connected? g)
	(define nodes (map car g))
	(define touched '())
	(connected-helper g (cdr g) nodes touched (car g)))
(define (connected-helper G unvisited nodes touched node)
	(cond
		[(equal? nodes touched) #t]
		[(contains? touched (car node)) #f]
		[else (or (map (connected-helper-caller G (cdr unvisited) nodes (cons (car node) touched)) (map (get-node G) (cdr node))))]))
(define (connected-helper-caller G unvisited nodes touched)
	(lambda (node)
		(connected-helper G unvisited nodes touched node)))
(define (get-node G)
	(lambda (elem)
		(if (null? G)
			'()
			(if (equal? elem (caar G))
				(car G)
				(get-node elem (cdr G))))))