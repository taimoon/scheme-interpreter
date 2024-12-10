(let ((sqr (lambda (x) (* x x))))
  (write (procedure? sqr)) (newline)
  (write (procedure? 7)) (newline)
  (write (+ (sqr 3) (sqr 4)))
  (newline))

(let* ((k 17)
       (f (lambda () k)))
  (write (f))
  (newline))

(let* ((sqr (lambda (x) (* x x)))
       (sos (lambda (x y) (+ (sqr x) (sqr y)))))
  (write (sos 3 4))
  (newline))

(let ((adder (lambda (x) (lambda (y) (+ x y)))))
  (write ((adder 10) 7))
  (newline))
