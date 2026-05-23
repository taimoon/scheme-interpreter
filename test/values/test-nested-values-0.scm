(call-with-values
  (lambda ()
    (call-with-values
      (lambda ()
        (values 2 3))
      (lambda (x y)
        (write x) (newline)
        (write y) (newline)
        (values x y))))
  (lambda xs (write xs) (newline)))