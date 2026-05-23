(call-with-values
  (lambda () (values 2 3 5 7))
  (lambda x (write x) (newline) 0))