(write
  (call/cc
    (lambda (k)
      (k 0)
      (write #f)
      (newline))))
(newline)
