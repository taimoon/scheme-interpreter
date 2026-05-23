(call-with-values
  (lambda () (values))
  (lambda x (write x) (newline)))