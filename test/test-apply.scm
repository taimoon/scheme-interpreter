(define (write-line x)
  (write x)
  (newline))

(apply write-line (list (apply (lambda (x y) (+ x y)) '(2 3))))

(apply write-line (list (apply list '(1 2 3 4))))

(let loop ((n 10) (acm 0))
  (if (<= n 0)
      (apply write-line (list acm))
      (apply loop (list (- n 1) (+ acm n)))))

(let ((f (lambda (x y . z)
          (if (< x y)
              z
              (cons x (cons y z))))))
  (define (%apply f xs) (apply f xs))
  (write-line (%apply f (list 1 2)))
  (write-line (%apply f (list -7 7 "reimu" "yes")))
  (write-line (%apply f (list 2 1 #t))))
