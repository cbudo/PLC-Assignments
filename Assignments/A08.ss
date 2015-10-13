; Christopher Budo
; Assignment 8
; September 25, 2015

; Problem 1
(define (slist-map proc slist)
	(let smap ([sl slist])
		(cond [(null? sl) '()]
			[(symbol? (car sl))
				(cons (proc (car sl))
					(smap (cdr sl)))]
			[else
				(cons (smap (car sl)) 
					(smap (cdr sl)))])))


(define (slist-reverse slist)
	(let srev ([sl slist])
		(cond [(null? sl) '()]
			[(symbol? (car sl))
				(append (srev (cdr sl))
					(list (car sl)))]
			[else
				(append (srev (cdr sl)) 
					(list (srev (car sl))))])))


(define (slist-paren-count slist)
	(+ 2 (let paren-count ([sl slist])
		(cond [(null? sl) 0]
			[(symbol? (car sl)) (paren-count (cdr sl))]
			[else
				(+ 2 (paren-count (cdr sl)) 
					(paren-count (car sl)))]))))

(define (slist-depth slist)
	(let go-deeper ([s1 slist] [depth 1])
		(cond [(null? s1) depth]
			[(symbol? (car s1)) (max depth (go-deeper (cdr s1) depth))]
			[else
				(max (go-deeper (car s1) (+ depth 1)) (go-deeper (cdr s1) depth))])))
(define (slist-symbols-at-depth slist d)
	(let go-deeper ([s1 slist] [depth 1])
		(cond [(null? s1) '()]
			[(symbol? (car s1))
				(if (eq? depth d)
					(append (list (car s1)) (go-deeper (cdr s1) depth))
					(go-deeper (cdr s1) depth))]
			[else
				(append (go-deeper (car s1) (+ depth 1)) (go-deeper (cdr s1) depth))])))

; Problem 2
(define (subst-leftmost new old slist eqality-pred?)
	(if (null? slist)
		'()
		(cdr (subst-leftmost-helper new old slist eqality-pred?))))
(define (subst-leftmost-helper new old slist eqality-pred?)
	(cond
		[(null? slist) (list #f)]
		[(or (number? slist) (symbol? slist)) (if (eqality-pred? old slist) (cons #t new) (cons #f slist))]
		[(pair? slist)
			(let [(getcar (subst-leftmost-helper new old (car slist) eqality-pred?))]
				(if (car getcar)
					(cons #t (cons (cdr getcar) (cdr slist)))
					(let [(getcdr (subst-leftmost-helper new old (cdr slist) eqality-pred?))]
						(if (car getcdr)
							(cons #t (cons (cdr getcar) (cdr getcdr)))
							(cons #f slist)))))]))