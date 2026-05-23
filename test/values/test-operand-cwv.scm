(let ((cwv (lambda () call-with-values)))
  (write
    ((cwv)
     (lambda () (values 11 13))
     +))
  (newline)
)