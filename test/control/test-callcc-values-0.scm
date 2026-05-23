(call-with-values
  (lambda () (call/cc (lambda (k) (k))))
  (lambda x (write x)))
(newline)
