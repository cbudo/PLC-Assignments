(define (reverse! lst)
	(if (null? lst)
		'()
		(let loop([lst lst]
			[prev '()]
			[next (cdr lst)])
			(set-cdr! lst prev)
			(if (null? next)
				lst
				(loop next lst (cdr next))))))


(define (make-stack)
	(let ([stk '()])
		(lambda (msg . args)
			(case msg
				[(empty?) (null? stk)]
				[(push)
					(set! stk (cons (car args) stk))]
				[(pop)
					(let ([top (cdr stk)])
						(set! stk (cdr stk))
						top)]
				[else
					(error
						'stack
						"illegal stack message: ~a"
						msg)]))))