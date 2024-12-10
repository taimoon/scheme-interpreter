(let ()
(define (map f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map f (cdr xs)))
      '()))

(define (write-line x)
  (write x) (newline))

(write-line
  (map integer? (list 2 3 5 (list 1 2 3) '() #t "something")))
)