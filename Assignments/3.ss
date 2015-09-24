; Chris Budo
; Assignment 3

;1
(define nearest-point
	(lambda (p list-of-points)
		(cond [(<= (distance p 
						(car list-of-points)) 
					(distance p 
						(cadr list-of-points))) 
				(if (<= (distance p (car list-of-points)) 
					(distance p (caddr list-of-points))) (car list-of-points) (caddr list-of-points))]
			[(<= (distance p (cadr list-of-points)) (distance p (caddr list-of-points))) (caddr list-of-points) (cadr list-of-points)]
			[else (caddr list-of-points)])))

;2
(define union
	(lambda (l1 l2)
		(cond [(equal? l1 '()) l2]
			[(equal? l2 '()) l1]
			[else (if (not (member (car l2) l1)) 
				(union (cons (car l2) l1) 
					(cdr l2))
				(union l1 (cdr l2)))])))


;3
(define intersection
	(lambda (s1 s2)
		(cond [(null? s1) '()]
			[(null? s2) '()]
			[(member (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))]
			[else (intersection (cdr s1) s2)])))

;4
(define subset?
	(lambda (s1 s2)
		(cond [(null? s1) #t]
			[(null? s2) #f]
			[else (and (member (car s1) s2) (subset? (cdr s1) s2))])))

;5
(define relation?
	(lambda (obj)
		(if (set? obj)
			(not (member #f (map mapEqual2 
				(map list-size obj))))
			#f)))

;6
(define domain
	(lambda (r)
		(remove-duplicates (map car r))))

;7
(define reflexive?
	(lambda (r)
		(reflexive-member? (pair (domain r)) r)))

;8
(define hailstone-step-count
	(lambda (n)
		(cond 
			[(or (eq? 0 n) (eq? 1 n)) 0]
			[(eq? 0 (modulo n 2)) 
				(+ 1 (hailstone-step-count (/ n 2)))]    
			[(eq? 1 (modulo n 2))
				(+ 1 (hailstone-step-count (+ (* 3 n) 1)))])))

; helper functions

(define list-min
	(lambda (li)
		(cond [(null? (cdr li)) (car li)]
			[(< (car li) (list-min li)) (car li)]
			[else (list-min (cdr li))])))

(define (remove-duplicates li)
  (cond ((null? li)
         '())
        ((member (car li) (cdr li))
         (remove-duplicates (cdr li)))
        (else
         (cons (car li) (remove-duplicates (cdr li))))))

(define mapEqual2
	(lambda (element) (equal? element 2)))

(define list-size
	(lambda (l1)
		(if (or (not (list? l1) ) (null? l1))
			0
			(+ 1 (list-size (cdr l1))))))

(define range
	(lambda (r)
		(remove-duplicates (map cadr r))))

(define pair
	(lambda (r)
		(if (null? r)
			'()
			(cons (list (car r) (car r)) (pair (cdr r))))))

(define reflexive-member?
	(lambda (r1 r2)
		(if (null? r1) 
			#t
			(and (member (car r1) r2) (reflexive-member? (cdr r1) r2)))))

; Brought over from previous assignment
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2)
				(car p1))
			(- (cadr p2)
				(cadr p1))
			(- (caddr p2)
				(caddr p1)))))


(define dot-product
	(lambda (vec1 vec2)
		(+ (* (car vec1)
				(car vec2))
			(* (cadr vec1)
				(cadr vec2))
			(* (caddr vec1)
				(caddr vec2)))))


(define vec-length
	(lambda (v)
		(inexact->exact (expt (+ (expt (car v)
				2)
			(expt (cadr v)
				2)
			(expt (caddr v)
				2))
			1/2))))


(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))


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

(define set?
	(lambda (ls)
		(if (not (list? ls)) #f
			(if (not (null? ls)) 
				(and (not (member (car ls) (
						cdr ls)) )
					(set? (cdr ls))) 
					#t))))