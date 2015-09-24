(define (Fahrenheit->Celsius fahrenheit)
	(* (- fahrenheit 32) 5/9))

(define interval-contains? 
	(lambda (interval number)
		(and (<= (car interval) 
				number) 
			(>= (car (cdr interval)) 
				number))))

(define interval-intersects?
	(lambda (i1 i2)
		(or (interval-contains? i2 (car i1))
			(interval-contains? i1 (car i2)))))

(define interval-union
	(lambda (i1 i2)
		(cond ((interval-contains? i1 (car i2))
				(if (interval-contains? i1 (cadr i2))
					(list i1)
					(list (list (car i1) (cadr i2)))))
			((interval-contains? i1 (cadr i2))
				(list (list (car i2) (cadr i1))))
			((interval-contains? i2 (car i1))
				(if (interval-contains i2 (cadr i1))
					i2))
			((interval-contains? i2 (cadr i1))
				(list (list (car i2) (cadr i1))))
			(else (list i1 i2)))))

(define divisible-by-7?
	(lambda (num)
		(eq? (modulo num 7) 0)))

(define ends-with-7?
	(lambda (num)
		(eq? (modulo num 10) 7)))

(define 1st
	(lambda (ls)
		(car ls)))

(define 2nd
	(lambda (ls)
		(car (cdr ls))))

(define 3rd
	(lambda (ls)
		(car (cddr ls))))
