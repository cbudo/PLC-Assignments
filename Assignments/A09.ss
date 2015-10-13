; Christopher Budo
; Assignment 9
; September 27, 2015

; Problem 1
(define (snlist-recur base proc-elem proc-ls)
	(letrec 
		((helper 
			(lambda (lst)
				(cond 
					[(null? lst) base]
					[(not (list? (car lst)))
						(proc-elem (car lst) (helper (cdr lst)))]
					[else 
						(proc-ls (helper (car lst)) 
							(helper (cdr lst)))]))))
			helper))
; a
(define (sn-list-sum snlst)
	((snlist-recur 0 + +) snlst))
; b
(define (sn-list-map proc snlst)
	((snlist-recur '() (lambda (cr ls) (cons (proc cr) ls)) cons) snlst))
; c
(define (sn-list-paren-count snlst)
	((snlist-recur 2 (lambda (first second) second) +) snlst))
; d
(define (sn-list-reverse snlst)
	((snlist-recur '() (lambda (first second) (append second (list first))) (lambda (first second) (append second (list first)))) snlst))
; e
(define (sn-list-occur s snlst)
	((snlist-recur 0 (lambda (first second) (if (eq? first s) (+ 1 second) second)) +) snlst))
; f
(define (sn-list-depth snlst)
	((snlist-recur 1 (lambda (first second) second) (lambda (first second) (+ (max first (- second 1)) 1))) snlst))

; Problem 2
(define (bt-recur proc-num proc-recur)
	(letrec ([helper 
		(lambda (bt)
			(if (number? bt)
				(proc-num bt)
				(if (null? bt)
					'()
					(proc-recur (car bt)
						(helper (cadr bt))
						(helper (caddr bt))))))])helper))
; a
(define (bt-sum T)
	((bt-recur + (lambda (this left right) (+ left right))) T))
; b
(define (bt-inorder T)
	((bt-recur (lambda (x) '()) (lambda (this left right) (append left (list this) right))) T))