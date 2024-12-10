(define (fact n)
  (let iter ((n n) (acm 1))
    (if (<= n 1)
        acm
        (iter (- n 1) (* n acm)))))

(write (fact 10))
(newline)