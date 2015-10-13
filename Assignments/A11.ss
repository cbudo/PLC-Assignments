; Christopher Budo
; Assignment 11
; October 8, 2015

; starting code
(load "C:/users/budocf/documents/courses/junior/csse304/assignments/chez-init.ss")
; datatypes
(define-datatype bintree bintree?
    (leaf-node
        (datum number?))
    (interior-node
        (key symbol?)
        (left-tree bintree?)
        (right-tree bintree?)))
        (define-datatype expression expression?
    (var-exp
        (id symbol?))
    (app-exp
        (rator expression?)
        (rand (list-of expression?)))
    (lit-exp
        (id scheme-value?))
    (lambda-exp
        (id (list-of expression?))
        (body (list-of expression?)))
    (no-parens-lambda-exp
        (id symbol?)
        (body (list-of expression?)))
    (improper-lambda-exp
        (ids expression?)
        (body (list-of expression?)))
    (let-exp
        (ids (list-of expression?))
        (values (list-of expression?))
        (body (list-of expression?)))
    (let*-exp
        (ids (list-of expression?))
        (values (list-of expression?))
        (body (list-of expression?)))
    (letrec-exp
        (ids (list-of expression?))
        (values (list-of expression?))
        (body (list-of expression?)))
    (named-let-exp
        (name expression?)
        (ids (list-of expression?))
        (values (list-of expression?))
        (body (list-of expression?)))
    (set!-exp
        (id symbol?)
        (body expression?))
    (if-exp
        (test expression?)
        (true expression?)
        (false expression?))
    (no-else-if-exp
        (test expression?)
        (true expression?)))

(define (scheme-value? val)
        #t)

; Problem 1
; Part a
(define-syntax my-let
  (syntax-rules ()
    [(my-let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...)]
    [(my-let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...)]))
; Part b
(define-syntax my-or
    (syntax-rules ()
        [(_) #f]
        [(_ e) e]
        [(_ e1 e2 ...)
            (let [(t e1)]
                (if t
                    t
                    (my-or e2 ...)))]))
; Part c
(define-syntax +=
    (syntax-rules ()
        [(+= v e1) (begin (set! v (+ e1 v)) v)]))
; Part d
(define-syntax return-first
    (syntax-rules ()
        [(_) '()]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
            (let ([a e1])
                (begin e2 e3 ... a))]))

; Problem 2
(define (bintree-to-list tree)
    (cases bintree tree
        [leaf-node (datum)
            (list 'leaf-node datum)]
        [interior-node (key left right)
        	(list
                'interior-node
                key
                (bintree-to-list left)
                    (bintree-to-list right))]))

; Problem 3
(define (max-interior tree)
    (car (max-interior-helper tree)))

(define (max-interior-helper tree)
    (cases bintree tree
      [leaf-node (datum) datum]
      [interior-node (key left right)
         (let ((l (max-interior-helper left)) (r (max-interior-helper right)))
           (cond
             [(and (integer? l) (integer? r))
                 (list key (+ l r) (+ l r))]
             [(integer? l)
                 (let ((rkey (list-ref r 0)) (r1 (list-ref r 1)) (r2 (list-ref r 2)))
                     (if (> r1 (+ r2 l))
                         (list rkey r1 (+ r2 l))
                         (list key (+ r2 l) (+ r2 l))))]
             [(integer? r)
                 (let ((lkey (list-ref l 0)) (l1 (list-ref l 1)) (l2 (list-ref l 2)))
                     (if (> l1 (+ l2 r))
                         (list lkey l1 (+ l2 r))
                         (list key (+ l2 r) (+ l2 r))))]
             [else
                (let ((curr (+ (list-ref l 2) (list-ref r 2)))
                     (rkey (list-ref r 0)) (r1 (list-ref r 1))
                     (lkey (list-ref l 0)) (l1 (list-ref l 1)))
                         (if (and (>= curr l1) (>= curr r1))
                             (list key curr curr)
                             (if (> l1 r1)
                                 (list lkey l1 curr)
                                 (list rkey r1 curr))))]))]))

; Problem 4
; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (parse-exp datum)
    (cond
        [(symbol? datum) (var-exp datum)]
        [(boolean? datum) (lit-exp datum)]
        [(string? datum) (lit-exp datum)]
        [(vector? datum) (lit-exp datum)]
        [(number? datum) (lit-exp datum)]
        [(not (proper-list? datum))
          (eopl:error 'parse-exp "Error in parse-exp: Improper list in ~s" datum)]
        [(pair? datum)
            (cond
                [(not (proper-list? datum)) (lit-exp datum)]

                [(eqv? (car datum) 'quote)
                    (if (equal? (length datum) 2)
                        (lit-exp datum)
                        (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax ~s" datum))]

                [(eqv? (car datum) 'lambda)
                    (if (> (length datum) 2)
                        (if (symbol? (cadr datum))
                            (no-parens-lambda-exp (cadr datum) (map parse-exp (cddr datum)))
                            (if (and (proper-list? (cadr datum))
                                (and (map symbol? (cadr datum)))
                                (not (list-contains-multiples? (cadr datum))))
                                    (lambda-exp (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))
                                    (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax ~s" datum)))
                        (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum))]

                [(eqv? (car datum) 'let)
                    (cond
                        [(and (> (length datum) 2) (proper-list? (cadr datum))
                            (and (map proper-list? (cadr datum)))
                            (and (map (lambda (ls) (equal? (length ls) 2)) (cadr datum)))
                            (and (map (lambda (ls) (symbol? (car ls))) (cadr datum)))
                            (not (list-contains-multiples? (map car (cadr datum)))))
                                (let-exp (map parse-exp (map car (cadr datum))) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))]
                        [else (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax ~s" datum)])]

         [(eqv? (car datum) 'let*)
              (if (and (> (length datum) 2) (proper-list? (cadr datum))
                      (and (map proper-list? (cadr datum)))
                      (and (map (lambda (ls) (equal? (length ls) 2)) (cadr datum)))
                      (and (map (lambda (ls) (symbol? (car ls))) (cadr datum)))
                      (not (list-contains-multiples? (map car (cadr datum)))))
                  (let*-exp (map parse-exp (map car (cadr datum)))
                      (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
                  (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax ~s" datum))]

         [(eqv? (car datum) 'letrec)
              (if (check-let datum)
                  (letrec-exp (map parse-exp (map car (cadr datum)))
                      (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
                  (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax ~s" datum))]

         [(eqv? (car datum) 'set!)
          (if (not (equal? (length datum) 3))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (set!-exp (cadr datum) (parse-exp (caddr datum))))]

         [(eqv? (car datum) 'if)
          (if (or (< (length datum) 3) (> (length datum) 4))
              (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)
              (if (null? (cdddr datum)) ;w/o else
                  (no-else-if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))
                  (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))))] ;if w/ else

         [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
        [else (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax ~s" datum)]))

(define (check-let datum)
    (cond
        [(null? (cadr datum))
            (eopl:error 'parse-exp "Error in parse-exp: No declarations made in ~s" datum)]
        [(null? (cddr datum))
            (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)]
        [(null? (caadr datum))
            (eopl:error 'parse-exp "Error in parse-exp: No declarations made in ~s" datum)]
        [(not (list? (caadr datum)))
            (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax, declarations must be lists in ~s" datum)]
        [(not (and (map (lambda (ls) (equal? (length ls) 2)) (cadr datum))))
            (eopl:error 'parse-exp "Error in parse-exp: not all length 2 in ~s" datum)]
        [(not (and (map symbol? (map car (cadr datum)))))
            (eop`l:error 'parse-exp "Error in parse-exp: Invalid concrete syntax, declarations must be symbols in ~s" datum)]
        [(list-contains-multiples? (map car (cadr datum)))
            (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax, declarations must not repeat symbols in ~s" datum)]
        [else #t]))

(define (proper-list? x)
    (cond
        [(null? x) #t]
        [(pair? x) (proper-list? (cdr x))]
        [else #f]))

(define (list-contains-multiples? ls)
    (cond
      [(null? ls) #f]
      [(member (car ls) (cdr ls)) #t]
      [else (list-contains-multiples? (cdr ls))]))

(define (unparse-exp exp)
    (cases expression exp
        [var-exp (id) id]
        [lit-exp (id) id]

        [let-exp (ids values body)
            (append (list 'let (stitch-let (map unparse-exp ids) (map unparse-exp values))) (map unparse-exp body))]
        [named-let-exp (name ids values body)
            (append
                (list
                    'let
                    (unparse-exp name)
                    (stitch-let (map unparse-exp ids) (map unparse-exp values)))
                (map unparse-exp body))]
        [let*-exp (ids values body)
            (append
                (list
                    'let*
                    (stitch-let (map unparse-exp ids) (map unparse-exp values)))
                (map unparse-exp body))]
        [letrec-exp (ids values body)
            (append
                (list
                    'letrec
                    (stitch-let (map unparse-exp ids) (map unparse-exp values)))
                (map unparse-exp body))]

        [lambda-exp (id body)
            (append
                (list 'lambda (map unparse-exp id))
                (map unparse-exp body))]
        [no-parens-lambda-exp (id body)
            (append (list 'lambda id)
                (map unparse-exp body))]
        [improper-lambda-exp (id body)
            (append (list 'lambda (unparse-exp id))
                (map unparse-exp body))]

        [set!-exp (id body)
            (list 'set! id (unparse-exp body))]

        [if-exp (test true false)
            (list 'if (unparse-exp test) (unparse-exp true) (unparse-exp false))]
        [no-else-if-exp (test true)
            (list 'if (unparse-exp test) (unparse-exp true))]

        [app-exp (rator rand)
            (append (list (unparse-exp rator)) (map unparse-exp rand))]

        [else (eopl:error 'parse-exp "Error in parse-exp: Invalid concrete syntax in ~s" exp)]))

(define (stitch-let ids vals)
    (if (null? ids)
        '()
        (cons
            (list (car ids) (car vals))
            (stitch-let (cdr ids) (cdr vals)))))

(define (andmap pred? l)
    (cond
        [(null? l) #t]
        [else
            (and (pred? (car l))
                (andmap pred? (cdr l)))]))
