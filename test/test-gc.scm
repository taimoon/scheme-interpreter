(set! iota-loop
  (lambda (i n)
    (if (< i n)
        (cons i (iota-loop (+ i 1) n))
        '())))

(set! loop
  (lambda (i)
    (if (< i 1000)
        (begin
          (iota-loop 0 100)
          (loop (+ i 1)))
        (iota-loop 0 10))))

(write (loop 0))
(newline)
