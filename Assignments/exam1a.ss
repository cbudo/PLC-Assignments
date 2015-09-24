; Christopher Budo
; Exam 1
; September 22, 2015

; Problem 1
(define (group-by-two ls)
	(if (null? ls)
		'()
		(if (null? (cdr ls))
			(list (list (car ls)))
			(cons (group (car ls) (cadr ls)) (group-by-two (cddr ls))))))
(define (group n . m)
	(if (equal? m '())
		(list n)
		(list n (car m))))

; Problem 2
(define (group-by-n ls n)
	(if (null? ls)
		'()
		(cons (first-n-items ls n) (group-by-n-rest (next-el ls n) n))))
(define (group-by-n-rest ls n)
	(if (null? ls)
		'()
		(cons (first-n-items ls n) (group-by-n-rest (next-el ls n) n) )))
(define (first-n-items ls n)
	(if (null? (cdr ls))
		ls
		(if (eq? n 1)
			(list (car ls))
			(cons (car ls) (first-n-items (cdr ls) (- n 1))))))
(define (next-el ls n)
	(if (null? ls)
		'()
		(if (eq? n 0)
			ls
			(next-el (cdr ls) (- n 1)))))


; Problem 3
; i want to use each member of the list separatly... 
; why use andmap when apply works?
(define (sorted? ls)
	(apply < ls))

(define (sorted ls)
	(andmap < (map cadr (complete ls))))

(define complete
	(lambda (ls)
		(pairs ls ls)))
(define pairs
	(lambda (ls1 ls2)
		(if (null? ls1)
			'()
			(cons (construct-pair (car ls1) ls2) (pairs (cdr ls1) ls2)))))
(define construct-pair
	(lambda (elem ls)
		(list elem (get-graph-pair elem ls))))
(define get-graph-pair
	(lambda (element ls)
		(if (null? ls)
			'()
			(if (equal? element (car ls))
				(get-graph-pair element (cdr ls))
				(cons (car ls) (get-graph-pair element (cdr ls)))))))