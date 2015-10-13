(define fib-memo
    (let ([max 1]
          [sofar '((1 . 1) (0 . 1))])
        (lambda (n)
            (if (<= n max)
                (cdr (assq n sofar))
                (let* ([v1 (fib-memo (- n 1))]
                       [v2 (fib-memo (- n 2))]
                       [v3 (+ v2 v1)])
                    (set! max n)
                    (set! sofar (cons (cons n v3) sofar))
                    v3)))))
