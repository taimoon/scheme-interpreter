(letrec ((odd
          (lambda (n) (if (eq? n 0) #f (even (- n 1)))))
         (even
          (lambda (n) (if (eq? n 0) #t (odd (- n 1))))))
  (write (and (even 2) (odd 3) (odd 5)))
  (newline))
