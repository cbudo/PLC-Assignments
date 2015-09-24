; Christopher Budo
; Exam 1b
; September 22, 2015

; Problem 4
(define (sorted2? ls)
	(apply < ls))

; Problem 5
(define (all-=? ls)
	(if (null? (cdr ls))
		#t
		(if (null? (car ls))
			#t
			(if (number? (car ls))
				(and (eq? (car ls) (cadr ls)) (all-=? (cdr ls)))
				#f))))
			
; Problem 6
(define (max-path-from-root-to-leaf t)
	(if (equal? '() t)
		0
		(if (and (equal? '() (get-right t)) (equal? '() (get-left t)))
			(get-elem t)
			(max (+ (get-elem t) (max-path-from-root-to-leaf (get-right t))) (+ (get-elem t) (max-path-from-root-to-leaf (get-left t)))))))
		
(define (max-path-sum t)
	(if (and (equal? '() (get-right t)) (equal? '() (get-left t)))
		(get-elem t)
		(max (+ (get-elem t) (max-path-sum (get-right t))) (+ (get-elem t) (max-path-sum (get-left t))))))

(define (get-right T)
	(if (not (list? T))
		'()
		(if (list? (cddr T))
			(caddr T)
			(cddr T))))
(define (get-left T)
	(if (not (list? T))
		'()
		(cadr T)))
(define (get-elem T)
	(if (list? T)
		(car T)
		T))
