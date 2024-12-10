(define (sum n acm)
  (if (<= n 0)
      acm
      (sum (- n 1) (+ n acm))))

(write (sum 1000 0))
(newline)