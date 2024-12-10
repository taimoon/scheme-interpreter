(define iota
  (case-lambda
    ((n) (iota 0 n 1))
    ((start end)
     (iota start end 1))
    ((start end step)
     (let recur ((start start))
        (if (>= start end)
            '()
            (cons start (recur (+ start step))))))))

(define (write-line x)
  (write x)
  (newline))

(write-line (iota 10))
(write-line (iota 3 10))
(write-line (iota 3 10 2))