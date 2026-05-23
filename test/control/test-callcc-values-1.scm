(call-with-values
  (lambda () (call/cc (lambda (k) (k 1))))
  (lambda x (write x)))
(newline)
