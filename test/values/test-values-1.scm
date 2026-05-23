(let ((writeln (lambda (x) (write x) (newline))))
  (call-with-values
    (lambda () (values 2 3 5))
    (lambda (x y z) (writeln x) (writeln y) (writeln z) 0)))