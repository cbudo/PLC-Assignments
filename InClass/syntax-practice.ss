(define-syntax my-if
	(syntax-rules (then else)
		[(_ e1 then e2) (if e1 e2)]
		[(_ e1 then e2 else e3) (if e1 e2 e3)]))

(define-syntax ++
	(syntax-rules ()
		[(++ v) (begin (set! v (+ 1 v)) v)]))

(define-syntax ++post
	(syntax-rules ()
		[(++post v) 
			(let ([temp v])
				(set! v (+ 1 v))
				temp)]))

(define-syntax for
	(syntax-rules (:)
		[(_ ((init ...) : test : update ...) body ...)
			(begin init ...
				(let loop()
					(if test
						(begin body ...
							update ...
							(loop)))))]))