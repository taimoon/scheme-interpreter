(define (fact n)
  (if (<= n 0)
      1
      (* n (fact (- n 1)))))

(define (add a b)
  (if (<= a 0)
      b
      (add (- a 1) (+ b 1))))

(write (fact 10))
(newline)
(write (add 13 6))
(newline)
(write (add (fact 10) 0))
(newline)