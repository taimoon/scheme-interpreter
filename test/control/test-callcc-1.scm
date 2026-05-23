(write
  (+ (* 4 4)
     (call/cc (lambda (k) (* 3 3)))))
(newline)