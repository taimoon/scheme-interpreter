(set! fact
  (lambda (n)
    (if (< n 2)
        1
        (* n (fact (- n 1))))))
(writeln (fact 10))