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
					[(null? l) '()]
					[else
						(map (lambda (ele)
							(if (not (proc (car l) ele))
								(set! lesser (cons ele lesser))
								(set! greater (cons ele greater)))) (cdr l))
								(append (qsort proc lesser)
									(cons (car l) (qsort proc greater)))])))))

; Problem 3
(define (connected? g)
	(define nodes (map car g))
	(define explored '())
	(connected-helper g (list (car g)) nodes explored))
(define (connected-helper G stack nodes explored)
	(if (null? stack)
		(if (chris-equal? nodes explored) #t #f)
		(let ([node (car stack)])
			(cond
				[(contains? explored (car node)) (connected-helper G (cdr stack) nodes explored)]
				[else (connected-helper G (append (cdr stack) (map (get-node G) (cadr node))) nodes (cons (car node) explored))]))))
(define (get-node G)
	(lambda (elem)
		(if (null? G)
			'()
			(if (equal? elem (caar G))
				(car G)
				((get-node (cdr G)) elem )))))

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
	(cond
		[(and (list? obj) (empty-BST? obj)) #t]
		[(or (not (list? obj))
			(not (number? (car obj)))
			(not (eq? (length obj) 3))
			(not (list? (cadr obj)))
			(not (list? (caddr obj)))
			(not (check-order (BST-inorder obj)))) #f]
		[(and (number? (car obj)) (BST? (cadr obj)) (BST? (caddr obj)))]))
(define (check-order lst)
	(cond
		[(null? (cdr lst)) #t]
		[(< (car lst) (cadr lst)) (check-order (cdr lst))]
		[(>= (car lst) (cadr lst)) #f]))
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
	(cond [(empty-BST? bst)
		(list num '() '())]
		[(equal? num (get-elem bst)) bst]
		[(< num (get-elem bst)) (list (get-elem bst) (BST-insert num (get-left bst)) (get-right bst))]
		[(> num (get-elem bst)) (list (get-elem bst) (get-left bst) (BST-insert num (get-right bst)))]))

(define (BST-inorder bst)
	(cond
		[(null? bst) '()]
		[else (append (BST-inorder (get-left bst)) (list (get-elem bst)) (BST-inorder (get-right bst)))]))

(define (BST-element bst)
	(if (null? bst)
		'()
		(get-elem bst)))
(define (BST-left bst)
	(if (null? bst)
		'()
		(get-left bst)))
(define (BST-right bst)
	(if (null? bst)
		'()
		(get-right bst)))
(define (BST-insert-nodes bst nums)
	(if (null? nums)
		bst
		(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))))
(define (BST-contains? bst num)
	(if (empty-BST? bst)
		#f
		(cond
			[(equal? (get-elem bst) num) #t]
			[(< (get-elem bst) num) (BST-contains? (get-right bst) num)]
			[(> (get-elem bst) num) (BST-contains? (get-left bst) num)])))


; Problem 6
(define (map-by-position fn-list arg-list)
	(map (lambda (fn item) (fn item)) fn-list arg-list))

; Problem 7
(define (bt-leaf-sum T)
	(if (not (list? T))
		(if (number? T)
			T
			0)
		(+ (bt-leaf-sum (get-right T))
			(bt-leaf-sum (get-left T)))))
(define (bt-inorder-list T)
	(cond
		[(number? T) '()]
		[else (append (bt-inorder-list (get-left T)) (list (get-elem T)) (bt-inorder-list (get-right T)))]))


(define (bt-max T)
	(if (null? T)
		'()
		(bt-max-helper T)))
(define (bt-max-helper T)
	(if (null? T)
		#f
		(apply max-catcher
			(chris-cons (bt-max-helper (get-left T))
				(chris-cons (bt-max-helper (get-right T))
					(list (get-elem T)))))))
(define (chris-cons item lst)
	(if item
		(cons item lst)
		lst))

(define (max-catcher el . rest)
	(if (null? rest)
		el
		(chris-max (cons el rest))))
(define (chris-max ls)
	(let ((head (car ls)) (tail (cdr ls)))
		(if (null? tail)
			(if (number? head)
				head
				0)
			(let ((max-in-tail (chris-max tail)))
				(if (> head max-in-tail)
					(if (number? head)
						head
						0)
					(if (number? max-in-tail)
						max-in-tail
						0))))))


(define (bt-max-interior T)
	(caddr (bt-max-int-call T)))
(define (bt-max-int-call T)
	(if (number? T)
		(list T T '())
		(bt-max-int-helper T)))
(define (bt-max-int-helper T)
	(define left (bt-max-int-call (cadr T)))
	(define right (bt-max-int-call (caddr T)))
	(cond [(number? T) T]
		[(and (> (+ (acr left) (car right)) (cadr left)) (> (+ (car left) (car right)) (cadr right)))
			(list (+ (car left) (car right)) (+ (car left) (car right)) (car T))]
		[(and (equal? '() (caddr right)) (equal? '() (caddr left)))
			(list (+ (car left) (car right)) (+ (car left) (car right)) (car T))]
		[(> (cadr left) (+ (car left) (car right)))
			(if (equal? '() (caddr left))
				(list (cadr left) (cadr left) (car T))
				(list (+ (car left) (car right)) (cadr left) (caddr left)))]
		[else (if (equal? '() (caddr right))
			(list (cadr right) (cadr right) (car T))
			(list (+ (car left) (car right)) (cadr right) (caddr right)))]))

; Helper Functions

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

(define (contains? slist sym)
	(let isin? ([sl slist])
		(cond [(null? sl) #f]
			[(symbol? (car sl))
				(or (eq? sym (car sl))
					(isin? (cdr sl)))]
			[else
				(or (isin? (car sl)) (isin? (cdr sl)))])))

(define chris-equal?
	(lambda (lst1 lst2)
		(and (eq? (length (intersection lst1 lst2)) (length lst1)) (eq? (length (intersection lst1 lst2)) (length lst2)))))
(define intersection
	(lambda (s1 s2)
		(cond [(null? s1) '()]
			[(null? s2) '()]
			[(member (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))]
			[else (intersection (cdr s1) s2)])))
