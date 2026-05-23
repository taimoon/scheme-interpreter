(let ((f (lambda (k) (k 0) (write #f) (newline))))
  (write (call/cc f))
  (newline))
