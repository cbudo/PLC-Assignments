(define fact
	(lambda (n)
		(if (zero? n)
			1
			(* n (fact (- n 1))))))


(define fact2
	(lambda (n)
		(fact-acc n 1)))

(define fact-acc
	(lambda (n acc)
		(if (zero? n)
			acc
			(fact-acc (- n 1) (* acc n)))))