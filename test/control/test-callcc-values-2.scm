(call-with-values
  (lambda () (call/cc (lambda (k) (k 1 2 3))))
  (lambda x (write x)))
(newline)
