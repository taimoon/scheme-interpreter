(write (list 1 2 3)) (newline)
(write (list)) (newline)
(write (list 1)) (newline)
(write (list (list 1) 2 (list #f))) (newline)

(let ()
  (define (f x y . z)
    (if (< x y)
        z
        (cons x (cons y z))))
  (write (f 1 2  #\a #\b #\c)) (newline)
  (write (f 7 3 #t #f)) (newline))