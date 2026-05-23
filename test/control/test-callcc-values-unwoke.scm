(call-with-values
  (lambda () (call/cc (lambda (k) 0)))
  (lambda (x) (write x)))
(newline)
