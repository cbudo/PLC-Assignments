; Chris Budo
; Assignment 5
; September 13, 2015

; Problem 1
(define minimize-interval-list
	(lambda (ls)
		(let ([lst (sort (lambda (i1 i2) (< (car i1) (car i2))) ls)])
			(letrec ([minimize-sorted-list
				(lambda (lst)
					(if (null? (cdr lst))
						lst
						(let* ([1st_ls (car lst)]
							[2nd_ls (cadr lst)]
							[1st_elem_1st_ls (car 1st_ls)]
							[2nd_elem_1st_ls (cadr 1st_ls)]
							[1st_elem_2nd_ls (car 2nd_ls)]
							[2nd_elem_2nd_ls (cadr 2nd_ls)])
					(if (>= 2nd_elem_1st_ls 1st_elem_2nd_ls)
						(minimize-sorted-list (cons (if (>= 2nd_elem_1st_ls 2nd_elem_2nd_ls)
										1st_ls
										(list 1st_elem_1st_ls 2nd_elem_2nd_ls))
									(cddr lst)))
						(cons (car lst) (minimize-sorted-list (cdr lst)))))))])
				(minimize-sorted-list lst)))))

; Problem 2
(define exists?
	(lambda (pred ls)
		(if (null? ls)
			#f
			(if (pred (car ls))
				#t
				(if (null? (cdr ls))
					#f
					(exists? pred (cdr ls)))))))

; Problem 3
(define list-index
	(lambda (pred ls)
		(if (null? ls)
			#f
			(if (member #t (map pred ls))
				(get-index #t (map pred ls))
				#f))))
(define get-index
	(lambda (val ls)
		(cond
			[(equal? val (car ls))
				0]
			[else (+ 1 (get-index val (cdr ls)))])))

; Problem 4
(define pascal-triangle
	(lambda (n)
		(if (< n 0)
			'()
			(cons (pascal-line n 0) (pascal-triangle (- n 1))))))

(define pascal-line
	(lambda (n m)
		(if (> m n)
			'()
			(cons (choose n m) (pascal-line n (+ 1 m))))))

; Problem 5
(define product
	(lambda (set1 set2)
		(if (or (null? set1) (null? set2))
			'()
			(append (connect (car set1) set2) (product (cdr set1) set2)))))
(define connect
	(lambda (x set2)
		(if (null? set2)
			'()
			(cons (list x (car set2)) (connect x (cdr set2))))))

; Problem 6
(define max-edges
	(lambda (n)
		(/ (* n (- n 1)) 2)))

; Problem 7
(define complete?
	(lambda (G)
		(define vecs (map car G))
		(if (or (null? vecs) (null? G))
			#t
			(check-vec vecs G))))

(define check-vec
	(lambda (vecs G)
		(define current-vec (car G))
		(if (null? (cdr G))
			#t
			(and (chris-equal? (list (remove-first (car current-vec) vecs)) (cdr current-vec)) (check-vec vecs (cdr G))))))
(define chris-equal?
	(lambda (lst1 lst2)
		(define ls1 (car lst1))
		(define ls2 (car lst2))
		(and (eq? (length (intersection ls1 ls2)) (length ls1)) (eq? (length (intersection ls1 ls2)) (length ls2)))))
		
; Problem 8
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

; Problem 9
(define replace
	(lambda (old new ls)
		(if (null? ls)
			'()
			(if (eq? (car ls) old)
				(cons new (replace old new (cdr ls)))
				(cons (car ls) (replace old new (cdr ls)))))))

; Problem 10
(define remove-first
	(lambda (element ls)
		(if (null? ls)
			'()
			(if (eq? (car ls) element)
				(cdr ls)
				(cons (car ls) (remove-first element (cdr ls)))))))

; Problem 11
(define remove-last
	(lambda (element ls)
		(reverse-list (remove-first element (reverse-list ls)))))

(define reverse-list
	(lambda (ls1)
		(if (null? ls1)
			'()
			(append (reverse-list (cdr ls1)) (list (car ls1))))))



; brought over from previous assignment

(define choose
	(lambda (n k)
		(/ (fact2 n) (* (fact2 k) (fact2 (- n k))))))

(define fact2
	(lambda (n)
		(fact-acc n 1)))

(define fact-acc
	(lambda (n acc)
		(if (zero? n)
			acc
			(fact-acc (- n 1) (* acc n)))))

(define intersection
	(lambda (s1 s2)
		(cond [(null? s1) '()]
			[(null? s2) '()]
			[(member (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))]
			[else (intersection (cdr s1) s2)])))