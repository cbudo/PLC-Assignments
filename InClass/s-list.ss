(define (contains? slist sym)
	(let isin? ([sl slist])
		(cond [(null? sl) #f]
			[(symbol? (car sl))
				(or (eq? sym (car sl))
					(isin? (cdr sl)))]
			[else
				(or (isin? (car sl)) (isin? (cdr sl)))])))


(contains? '() 'a)                          ; #f
(contains? '(b a) 'a)                       ; #t
(contains? '(( b (a)) ()) 'a)               ; #t
(contains? '((c b ()) ( b (c a)) ()) 'a)    ; #t
(contains? '((c b ()) ( b (c a)) ()) 'p)    ; #f


(define  (count-occurrences slist sym)
	(let count ([sl slist])
		(cond [(null? sl) 0]
			[(symbol? (car sl))
				(+ (if (eq? sym (car sl)) 1 0)
					(count (cdr sl)))]
			[else
				(+ (count (car sl)) (count (cdr sl)))])))



(count-occurrences '() 'a)                      ; 0
(count-occurrences '(b a b () a b) 'a)          ; 2
(count-occurrences '(b a b () a b) 'a)          ; 2
(count-occurrences '(b ((a) a) b () a b) 'a)    ; 3
(count-occurrences '((b ((a) a) b () a b)) 'a)  ; 3 



(define (notate-depth slist) 



(notate-depth '())                          ; ()
(notate-depth '(a))                         ; ((a 0)) 
(notate-depth '((a b) c))                   ; (((a 1) (b 1)) (c 0))
(notate-depth '( () (a (b)) c ((d) () e)))  ; (() ((a 1) ((b 2))) (c 0) (((d 2)) () (e 1)))
(notate-depth '((() (a (b)) c ((d) () e)))) ; ((() ((a 2) ((b 3))) (c 1) (((d 3)) () (e 2))))

(define  (flatten slist)




(flatten '( () (a ((b) c () ((d e ((f))) g) h)) ()))  ; (a b c d e f g h)



(define  (subst s1 s2 slist)




(subst 'a 'b '(() a (c ((a) a) (c (((c a)))))))  ; (() b (c ((b) b) (c (((c b)))))
  

