(let ()
(define (calc x)
  (match x
    (,() (guard (integer? x)) x)
    ((+ ,x ,y) (+ (calc x) (calc y)))
    ((* ,x ,y) (* (calc x) (calc y)))
    ((- ,x ,y) (- (calc x) (calc y)))))

(write
  (calc '(+ (* 3 3) (* 4 4))))
(newline)

(write
  (calc '(+ (* (- 4 1) 3) (* 4 (+ 3 1)))))
(newline)
)