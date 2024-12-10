(let* ((make-counter
        (lambda (init)
          (lambda ()
            (set! init (+ init 1))
            init)))
       (counter (make-counter 0)))
  (write (counter)) (newline)
  (write (counter)) (newline)
  (write (counter)) (newline)
  (write (counter)) (newline))

(let* ((make-counter
        (lambda (init)
          (lambda (inc)
            (set! init (+ init inc))
            init)))
       (counter (make-counter 0)))
  (write (counter 2)) (newline)
  (write (counter 3)) (newline)
  (write (counter 5)) (newline)
  (write (counter 7)) (newline))

