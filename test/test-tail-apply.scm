(define (write-line x)
  (write x)
  (newline))

(let loop ((x 0) (y 1000))
  (if (<= y 0)
      (apply write-line (list x))
      (apply loop (list (+ x y) (- y 1)))))
