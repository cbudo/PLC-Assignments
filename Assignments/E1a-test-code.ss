;; Test code for CSSE 304 Assignment 7

(define (test-group-by-two)
    (let ([correct '(()
		     ((a))
		     ((a b))
		     ((a b) (c))
		     ((a b) (c d) (e f) (g))
		     ((a b) (c d) (e f) (g h))
		     )]
          [answers 
            (list (group-by-two '())
		  (group-by-two '(a))
		  (group-by-two '(a b))
		  (group-by-two '(a b c))
		  (group-by-two '(a b c d e f g))
		  (group-by-two '(a b c d e f g h))
	     )])
      (display-results correct answers equal?)))

(define (test-group-by-n)
    (let ([correct '(()
		     ((a b c) (d e f) (g))
		     ((a b c d) (e f g))
		     ((a b c d) (e f g h))
		     ((a b c d e f g) (h i j k l m n) (o))
		     ((a b c d e f g h))
		     ((a b c d e f g h i j k l m n o p q) (r s t))
		     )]
          [answers 
            (list (group-by-n '() 3)
	    (group-by-n '(a b c d e f g) 3)
	    (group-by-n '(a b c d e f g) 4)
	    (group-by-n '(a b c d e f g h) 4)
	    (group-by-n '(a b c d e f g h i j k l m n o) 7)
	    (group-by-n '(a b c d e f g h) 17)
	    (group-by-n '(a b c d e f g h i j k l m n o p q r s t) 17)
	     )])
      (display-results correct answers equal?)))

(define (test-sorted?)
    (let ([correct '(#t
		     #t
		     #t
		     #t
		     )]
          [answers 
            (list (and (sorted? '(4)) (not (sorted? '(2 1 3 4 5))))
		  (and (sorted? '(1 2 3 4 5)) (not (sorted? '(1 2 3 5 4))))
		  (and (sorted? '(3)) (not (sorted? '(1 2 3 5 4))))
		  (and (sorted? '(1 2)) (not (sorted? '(1 2 4 3 3))))
	     )])
      (display-results correct answers equal?)))




;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'group-by-two) 
  (test-group-by-two )
  (display 'group-by-n) 
  (test-group-by-n)
  (display 'sorted?) 
  (test-sorted?)
)

(define r run-all)

