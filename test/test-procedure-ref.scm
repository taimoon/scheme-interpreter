(define (write-line x)
  (write x)
  (newline))

(define (neg x)
  (- 0 x))

(define (identity x) x)

(let ((x -17))
  (write-line ((if (< x 0) neg identity) x)))