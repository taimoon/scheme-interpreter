(let iter ((n 10) (acm 1))
  (cond
    ((<= n 1)
     (write acm) (newline)
     acm)
    (else
     (iter (- n 1) (* acm n)))))

(define (fact n)
  (cond
    ((<= n 1)
     n)
    (else
     (* n (fact (- n 1))))))

(write (fact 10)) (newline)
