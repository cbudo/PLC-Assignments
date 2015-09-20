; Christopher Budo
; Assignment 7

; Problem 1
(define (vector-append-list v lst)
	(define vectorAppend (make-vector (+ (vector-length v) (length lst))))
	(add-vector vectorAppend v 0)
	(add-list vectorAppend lst (vector-length v)))

(define (add-vector vect vectAp start)
	(cond [(eq? (vector-length vectAp) 0) ]
		[(< start (vector-length vectAp)) 
			(vector-set! vect start (vector-ref vectAp start))
			(add-vector vect vectAp (+ start 1))]))
	
(define (add-list vect lst n)
	(if (not (null? lst))
		(vector-set! vect n (car lst)))
	(if (null? lst)
		vect
		(add-list vect (cdr lst) (+ n 1))))


; Problem 2
(define qsort
	(lambda (proc l)
		(let ((lesser '()))
			(let ((greater '()))
				(cond
					((null? l) '())
					(else (map (lambda (ele)
						(if (not (proc (car l) ele))
							(set! lesser (cons ele lesser))
							(set! greater (cons ele greater)))) (cdr l))
					(append (qsort proc lesser) (cons (car l) (qsort proc greater))))
					)))))

; Problem 3
(define (connected? g)
	(define nodes (map car g))
	(or (map (check-complete nodes) g)))
(define (check-complete lst)
	(lambda (node)
		(if (eq? (length (cdr node))
				(- (length lst) 1))
			#t
			#f)))
(define (get-node elem G)
	(if (null? G)
		'()
		(if (equal? elem (caar G))
			(car G)
			(get-node elem (cdr G)))))

; Problem 4
(define (reverse-it lst) 
	(letrec ([helper 
		(lambda (ls acc)
			(if (null? ls)
				acc
				(helper (cdr ls) (cons (car ls) acc))))])
	(helper lst '())))

; Problem 5
(define (BST? obj)
	#f)
(define (BST obj)
	(if (not (list? obj))
		#f
		(if (null? obj)
			#f
			(and (check-right-tree (car obj) (caddr obj))
				(check-left-tree (car obj) (cadr obj))))))
(define (check-right-tree element right-tree) 
	(if (null? right-tree)
		#t
		(and (< element (car right-tree)) 
			(and (check-right-tree (car right-tree) (caddr right-tree))
				(check-left-tree (car right-tree) (cadr right-tree))))))
(define (check-left-tree element left-tree)
	(if (null? left-tree)
		#t
		(and (> element (car left-tree))
			(and (check-right-tree (car left-tree) (caddr left-tree))
				(check-left-tree (car left-tree) (cadr left-tree))))))
(define (empty-BST)
	'())

(define (empty-BST? obj)
	(equal? obj (empty-BST)))

(define (BST-insert num bst)
	(if (empty-BST? bst)
		(list num '() '())
		))

(define (BST-inorder bst)
	'())

(define (BST-element bst)
	'());(car bst))
(define (BST-left bst)
	'());(caadr bst))
(define (BST-right bst)
	'());(caddr bst))
(define (BST-insert-nodes bst nums)
	'())
(define (BST-contains? bst num)
	'())


; Problem 6
(define (map-by-position fn-list arg-list)
	(map (lambda (fn item) (fn item)) fn-list arg-list))

; Problem 7
(define (bt-leaf-sum T)
	0)

(define (bt-inorder-list T)
	'())

(define (bt-max T)
	(max T))

(define (bt-max-interior T)
	'())

; Helper Functions