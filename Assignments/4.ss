; Chris Budo
; Assignment 4
; CSSE 304


; Problem 1
(define multi-set?
	(lambda (obj)
		(if (and (relation? obj) (set? (map car obj)))
			(if (member #f (map (lambda (n) (> n 0)) (map cadr obj))) #f #t)
			#f)))


; Problem 2
(define ms-size 
	(lambda (ms)
		(apply + (map cadr ms))))


; Problem 3
(define matrix-ref
	(lambda (m row col)
		(if (eq? row 0)
			(get-list-el (car m) col)
			(matrix-ref (cdr m) (- row 1) col))))
(define get-list-el
	(lambda (li n)
		(if (eq? n 0)
			(car li)
			(get-list-el (cdr li) (- n 1)))))


; Problem 4
(define matrix?
	(lambda (obj)
		(cond [(not (list? obj)) #f]
			[(member #f (map list? obj)) #f]
			[(null? (car obj)) #f]
			[else (if (eq? (length obj) 1) #t (apply-eq? (map length obj)))])))
(define apply-eq?
	(lambda (ls)
		(eq? 1 (length (remove-duplicates ls)))))


; Problem 5
(define matrix-transpose
	(lambda (m)
		(apply map list m)))


; Problem 6
(define last
	(lambda (ls)
		(if (eq? 1 (length ls))
			(car ls)
			(last (cdr ls)))))


; Problem 7
(define all-but-last
	(lambda (lst)
		(if (eq? (length (cdr lst)) 0)
			'()
			(cons (car lst) (all-but-last (cdr lst))))))

; brought over from previous assignment

(define set?
	(lambda (ls)
		(if (not (list? ls)) #f
			(if (not (null? ls)) 
				(and (not (member (car ls) (
						cdr ls)) )
					(set? (cdr ls))) 
					#t))))

(define domain
	(lambda (r)
		(if (list? r)
			(remove-duplicates (map car r))
			'())))
(define relation?
	(lambda (obj)
		(if (set? obj)
			(not (member #f (map mapEqual2 
				(map list-size obj))))
			#f)))
(define mapEqual2
	(lambda (element) (equal? element 2)))
(define list-size
	(lambda (l1)
		(if (or (not (list? l1) ) (null? l1))
			0
			(+ 1 (list-size (cdr l1))))))

(define (remove-duplicates li)
  (cond ((null? li)
         '())
        ((member (car li) (cdr li))
         (remove-duplicates (cdr li)))
        (else
         (cons (car li) (remove-duplicates (cdr li))))))