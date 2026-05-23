(let ((f (lambda (k) 0)))
  (write (call/cc f))
  (newline))
