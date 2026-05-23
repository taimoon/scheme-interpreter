(let ()
  (call-with-values
    (lambda () 0)
    (lambda (x) (write x) (newline)))
)