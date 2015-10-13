(define-syntax my-and
	(syntax-rules ()
		[(_) #t]
		[(_ e) e]
		[(_ e1 e2 ...)
			(if e1
				(my-and e2 ...)
				#f)]))
(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_)]))
(define iszero? null?)
(define pred cdr)
(define succ
	(lamdba (n)
		(cons #t n)))
(define add append)