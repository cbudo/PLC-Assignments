(define list-sum
	(letrec ([helper
		(lambda (ls)
			(if (null? ls)
				0
				(+ (car ls)
					(helper (cdr ls)))))])
	helper))

(define list-prod
	(letrec ([helper
		(lambda (ls)
			(if (null? ls) 
				1
				(* (car ls)
					(helper (cdr ls)))))])
	helper))

(define map
	(lambda (proc ls)
		((apply-to-all proc) ls)))
(define (apply-to-all proc)
	(letrec([helper
		(lambda (ls)
			(if (null? ls)
				'()
				(cons (proc (car ls)) 
					(helper (cdr ls)))))])
	helper))


(define member?
	(lambda (item ls)
		((member?-c item) ls)))

(define (member?-c item)
	(letrec 
		([helper
		(lambda (ls)
			(if (null? ls)
				#f
				(or (equal? (car ls) item)
					(helper (cdr ls)))))])
	helper))

(define (list-recur base action)
	(letrec ([helper
		(lambda (ls)
			(if (null? ls)
				base
				(action (car ls) (helper (cdr ls)))))])
	helper))
(define list-sum-c (list-recur 0 +))
(define list-prod-c (list-recur 1 *))
(define (apply-to-all-c proc)
	(list-recur '()
		(lambda (x y)
			(cons (proc x)
				y))))
(define (member?-c-new item)
	(list-recur #f
		(lambda (x y)
			(or (equal? x item)
				y))))