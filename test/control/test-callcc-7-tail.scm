(let ((f (lambda () (call/cc (lambda (k) (k 123))))))
  (write (f))
  (newline))