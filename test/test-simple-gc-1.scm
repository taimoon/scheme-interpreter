(define (write-line x)
  (write x)
  (newline))

(define (length es)
  (if (pair? es)
      (+ 1 (length (cdr es)))
      0))

(define (f)
  (let loop ((obj '()) (i 0))
    (if (>= i 11)
        obj
        (loop (cons i obj) (add1 i)))))

(let loop ((i 0) (len (length (f))))
  (if (< i 5000)
      (if (= len 11)
          (loop (add1 i) (length (f)))
          (write-line len))
      (write-line (f))))