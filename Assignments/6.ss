; Christopher Budo
; Assignment 6
; Sept. 15, 2015

; Problem 1
(define curry2
	(lambda (pred)
		(lambda (inp1)
			(lambda (inp2)
				(pred inp1 inp2)))))

; Problem 2
(define curried-compose
	(lambda (pred1)
		(lambda (pred2)
			(lambda (inp1)
				(pred1 (pred2 inp1))))))

; Problem 3
(define compose
	(lambda fs
		(define (apply-all fs x)
			(if (null? fs)
				x
				((car fs) (apply-all (cdr fs) x))))
		(lambda (x) (apply-all fs x))))

; Problem 4
(define make-list-c
	(lambda (n)
		;(let repeat [n lst] 
		;	(if (eq? n 0)
		;		'()
		(lambda (lst)
			(make-list n lst))))
(define make-list
	(lambda (n lst)
		(if (eq? n 0)
			'()
			(cons lst  (make-list (- n 1) lst)))))

; Problem 5

(define let->application
	(lambda (le)
		(cons  (cons 'lambda (cons (map car (cadr le)) (cddr le))) (map cadr (cadr le) ))))

; Problem 6

(define let*->let
	(lambda (le)
		(append (pull-out (cadr le) #t) (cddr le))))

(define pull-out
	(lambda (ls first)
		(if (null? (cdr ls))
			(if first
				(cons 'let (list (list (car ls))))
				(cons 'let (list (list (car ls)))))
			(if first
				(append (cons 'let (list (list (car ls)))) (list (pull-out (cdr ls) #f)))
				(append (cons 'let (list (list (car ls)))) (pull-out (cdr ls) #f))))))
;(define get-parens
;	(lambda (n)
;		(if (eq? n 1)
;			'()
;			(cons (string->symbol ) (get-parens (- n 1))))))
;(get-parens (length (cadr le)))

; Problem 7

(define filter-in
	(lambda (pred? lst)
		(if (null? lst)
			'()
			(if (pred? (car lst))
				(cons (car lst) (filter-in pred? (cdr lst)))
				(filter-in pred? (cdr lst))))))

; Problem 8
(define filter-out
	(lambda (pred? lst)
		(if (null? lst)
			'()
			(if (not (pred? (car lst)))
				(cons (car lst) (filter-out pred? (cdr lst)))
				(filter-out pred? (cdr lst))))))

; Problem 9
(define sort-list-of-symbols
	(lambda (los)
		(map string->symbol (list-sort string<? (map symbol->string los)))))

; Problem 10

(define invert
	(lambda (lst)
		(if (null? lst)
			'()
			(cons (invert-pair (car lst)) (invert (cdr lst))))))
(define invert-pair
	(lambda (ls)
		(list (cadr ls) (car ls))))

; Problem 11
(define vector-index
	(lambda (pred? ls)
		(if (vector? ls)
			(vect-ind-helper pred? (vector->list ls) 0)
			(vect-ind-helper pred? ls 0))))
(define vect-ind-helper
	(lambda (pred? ls n)
		(if (null? ls)
			#f
			(if (pred? (car ls))
				n
				(vect-ind-helper pred? (cdr ls) (+ n 1))))))