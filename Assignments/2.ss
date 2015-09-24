; Chris Budo Assignment 2

;1 a,b
(define fact
	(lambda (n)
		(cond 
			((<= n 1) 1)
			(else (* n (fact (- n 1)))))))
(define choose
	(lambda (n k)
		(/ (fact n) (* (fact k) (fact (- n k))))))

;2
(define range
	(lambda (m n)
		(cond ((< m (- n 1)) (cons m (range (+ m 1) n)))
			((< m n) (list m))
			(else (list)))))

;3
(define set?
	(lambda (ls)
		(if (not (null? ls))
			(and (not (member (car ls) (cdr ls)) )
				(set? (cdr ls))) 
				#t)))

;4
(define sum-of-squares
	(lambda (lon)
		(if (eq? (length lon) 0)
			0
			(+ (expt (car lon) 2)
				(sum-of-squares (cdr lon))))))

;5
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2)
				(car p1))
			(- (cadr p2)
				(cadr p1))
			(- (caddr p2)
				(caddr p1)))))

;6
(define dot-product
	(lambda (vec1 vec2)
		(+ (* (car vec1)
				(car vec2))
			(* (cadr vec1)
				(cadr vec2))
			(* (caddr vec1)
				(caddr vec2)))))

;7
(define vec-length
	(lambda (v)
		(inexact->exact (expt (+ (expt (car v)
				2)
			(expt (cadr v)
				2)
			(expt (caddr v)
				2))
			1/2))))

;8
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

;9
(define cross-product
	(lambda (v1 v2)
		(list (- (* (cadr v1)
					(caddr v2))
				(* (caddr v1)
					(cadr v2)))
			(- (* (caddr v1)
					(car v2))
				(* (car v1)
					(caddr v2)))
			(- (* (car v1)
					(cadr v2))
				(* (cadr v1)
					(car v2))))))

;10
(define parallel?
	(lambda (v1 v2)
		(= (/ (car v2)
				(car v1))
			(/ (cadr v2)
				(cadr v1))
			(/ (caddr v2)
				(caddr v1)))))

;11
(define collinear?
	(lambda (p1 p2 p3)
		(=  0
			(vec-length (cross-product (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))))))