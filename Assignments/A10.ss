; Christopher Budo
; Assignment 10
; October 1, 2015

; Problem 1
(define (make-slist-leaf-iterator slist)
	(define stack (make-stack))
	(stack 'push slist)
	(popper stack))

(define (popper stack)
	(lambda ()
		(let leaf ([pop (stack 'pop)])
			(cond
				[(null? pop) (leaf (stack 'pop))]
				[(eq? pop #f) #f]
				[(list? pop) (stack 'push (cdr pop)) (leaf (car pop))]
				[else pop]))))

; Problem 2
(define (free-vars LcExp)
	(let findFree ([expression LcExp] [context '()] [free '()])
		(cond
			[(null? expression) free]
			[(valid-var? expression)
				(if (not (member expression context))
					(if (not (member expression free))
						(cons expression free)
						free)
					free)]
			[(eqv? 'lambda (car expression))
				(findFree (caddr expression) (append context (cadr expression)) free)]
			[else (findFree (cdr expression) context (findFree (car expression) context free))])))
(define (valid-var? x)
	(and (not (null? x)) (not (pair? x))))

(define (bound-vars LcExp)
	(let findBound ([expression LcExp] [context '()] [bound '()])
		(cond
			[(null? expression) bound]
			[(valid-var? expression)
				(if (member expression context)
					(if (not (member expression bound))
						(cons expression bound)
						bound)
					bound)]
			[(eqv? 'lambda (car expression))
				(findBound (caddr expression) (append context (cadr expression)) bound)]
			[else (findBound (cdr expression) context (findBound (car oexpression) context bound))])))
; Problem 3
(define (occurs-free? var exp)
  	(if (equal? var 'set!)
  	    #f
  	    (cond
			[(symbol? exp) (eqv? var exp)]
			[(null? (cdr exp)) #t]
			[(eqv? (car exp) 'lambda)
				(and (not (member var (cadr exp)))
					(occurs-free? var (caddr exp)))]
			[(eqv? (car exp) 'if)
				(or (occurs-free? var (cadr exp))
					(occurs-free? var (caddr exp)))]
			[(eqv? (car exp) 'let)
				(or (not (iscar var (cadr exp)))
					(occurs-free? var (caddr exp)))]
			[(eqv? (car exp) 'let*)
				(and (not (iscar var (cadr exp)))
					(or (not (show-up var (caddr exp)))
						(occurs-free? var (caddr exp))))]
			[else
				(or (occurs-free? var  (car exp))
					(occurs-free? var (cadr exp)))])))


(define (occurs-bound? var exp)
  	(if (equal? var 'set!)
  	    #f
  	    (cond
	      [(symbol? exp) #f]
	      [(null? (cdr exp)) #f]
	      [(eqv? (car exp) 'lambda)
		      (or (occurs-bound? var (caddr exp))
		           (and (member var (cadr exp))
		                (occurs-free? var (caddr exp))))]
	      [(eqv? (car exp) 'if) (or (occurs-bound? var (cadr exp)) (occurs-bound? var (caddr exp)))]
	      [(eqv? (car exp) 'let) (or (iscar var (cadr exp)) (occurs-bound? var (caddr exp)))]
	      [(eqv? (car exp) 'let*)
			  (or (and (iscar var (cadr exp)) (iscadr var (cadr exp)))
			      (and (iscar var (cadr exp))
				      (or (show-up var (caddr exp))
						  (occurs-bound? var (caddr exp)))))]
	      [else (or (occurs-bound? var  (car exp))
	                (occurs-bound? var (cadr exp)))])))
(define (show-up var ls)
  (if (null? ls)
      #f
      (if (symbol? ls)
          (if (equal? var ls)
              #t
              #f)
          (if (list? (car ls))
          (or (show-up var (car ls)) (show-up var (cdr ls)))
          (show-up var (cdr ls))))))
(define (iscar var ls)
	(if (null? ls)
	    #f
	    (if (equal? var (caar ls))
	        #t
	        (iscar var (cdr ls)))))

(define (iscadr var ls)
	(if (null? ls)
	    #f
	    (if (equal? var (cadar ls))
	        #t
	        (iscadr var (cdr ls)))))
; Problem 4
(define (lexical-address e)
  (lexical-helper e '()))

(define (lexical-helper exp bound)
	(cond
		[(symbol? exp)
			(if (number? (cadr (find-pos-depth exp bound 0)))
				(find-pos-depth exp bound 0)
				(append (find-pos-depth exp bound 0) (list exp)))]
		[(equal? 'lambda (car exp))
			(list
				'lambda
				(cadr exp)
				(lexical-helper (caddr exp) (cons (cadr exp) bound)))]
		[(equal? 'if (car exp))
			(list
				'if
				(lexical-helper (cadr exp) bound)
				(lexical-helper (caddr exp) bound)
				(lexical-helper (cadddr exp) bound))]
		[else
			(cons (lexical-helper (car exp) bound)
				(rec-sort-helper (cdr exp) bound))]))

(define (find-pos-depth exp bound base)
	(cond
		[(null? bound) (list ': 'free)]
		[else
			(let ([pos (find-pos exp (car bound) 0)])
				(if pos (list ': base pos)
					(find-pos-depth exp (cdr bound) (+ 1 base))))]))

(define (find-pos exp bound pos)
	(cond
		[(null? bound) #f]
		[else
			(if (equal? exp (car bound))
				pos
				(find-pos exp (cdr bound) (+ 1 pos)))]))

(define (rec-sort-helper exp bound)
	(if (null? exp)
		'()
		(cons (lexical-helper (car exp) bound)
			(rec-sort-helper (cdr exp) bound))))


; Problem 5
(define (un-lexical-address e)
  (let un-lexical-helper ([exp e] [bound '()])
	(cond
		[(null? exp) '()]
		[(and (list? exp) (equal? 1 (length exp)))
			(un-lexical-helper (car exp) bound)]
		[(equal? (car exp) ':)
			(if (equal? (cadr exp) 'free)
				(list (caddr exp))
				(list (find-dp bound (cadr exp) (caddr exp))))]
		[(equal? (car exp) 'if)
			(append
				(list 'if)
				(if (not (equal? ': (caadr exp)))
					(list (un-lexical-helper (cadr exp) bound))
					(un-lexical-helper (cadr exp) bound))
                 (un-lexical-helper (cddr exp) bound))]
		[(equal? (car exp) 'lambda)
			(list
				'lambda
				(cadr exp)
				(un-lexical-helper (caddr exp) (cons (cadr exp) bound)))]
		[else
			(let ((temp (un-lexical-helper (car exp) bound))
		    	(temp2 (un-lexical-helper (cdr exp) bound)))
				(append
					(if (not (equal? ':  (caar exp)))
						(list temp)
					  	temp)
				  	(if (not (equal? ': (caadr exp)))
				  		(list temp2)
						temp2)))])))

(define (find-dp lst d p)
	(cond
		[(zero? d) (find-p (car lst) p)]
		[else (find-dp (cdr lst) (- d 1) p)]))

(define (find-p lst p)
	(cond
		[(null? lst) lst]
		[(zero? p) (car lst)]
		[else (find-p (cdr lst) (- p 1))]))



; Helper functions
(define (make-stack)
	(let ([stk '()])
		(lambda (msg  . args )
			(case msg
			[(empty?) (null? stk)]
			[(push)   (set! stk (cons (car args) stk))]
			[(pop)    (if (null? stk)
							#f
							(begin (let ([top (car stk)])
								(set! stk (cdr stk))
								top)))]
			[else (errorf 'stack "illegal message to stack object: ~a" msg)]))))

(define (let->application le)
		(cons  (cons 'lambda (cons (map car (cadr le)) (cddr le))) (map cadr (cadr le) )))
(define (let*->let original)
	(append
		(list 'let)
		(let*-helper (cadr (car original)) (list (car (reverse (car original)))))))
(define (let*-helper cur last)
	(if (= 1 (length cur))
		(list cur (car last))
		(list
			(list (car cur))
			(append
				(list 'let)
				(let*-helper (cdr cur) last)))))
(define (list-index val ls)
	(let find ([lst ls] [counter 0])
		(if (null? lst)
			0
			(if (eq? (car lst) val)
				counter
				(find (cdr lst) (+ 1 counter))))))
