(let ((val (lambda () values)))
  (call-with-values
    (lambda ()
      ((val) 2 3 5))
    (lambda xs (write xs) (newline))))