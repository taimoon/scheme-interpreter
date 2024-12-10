(define (write-line x)
  (write x)
  (newline))

(define (max x y)
  (if (< x y) y x))

(define (ordered? a b c d)
  (and (< a b) (< b c) (< c d)))

(write-line 29)
(write-line 17)
(write-line -17)
(write-line (ordered? 2 3 5 7))