(define fn
  (let ((x -17)
        (y -23)
        (z -29))
    (case-lambda
      ((a)
       (+ x a))
      ((a b)
       (+ (+ a b) (+ x y)))
      ((a b c)
       (+ (+ a b) (+ x (+ c z)))))))

(define (write-line x)
  (write x)
  (newline))

(write-line (fn 29))
(write-line (fn 29 31))
(write-line (fn 29 31 37))
