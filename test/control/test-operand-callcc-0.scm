(let ((f (lambda () call/cc)))
  (write
    ((f)
      (lambda (k)
       (k 0)
       (write #t)
       (newline))))
  (newline)
  )