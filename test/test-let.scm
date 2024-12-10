(write
  (let ((x 3) (y 4) (z (* 5 5)))
    (- z (+ (* x x) (* y y)))))
(newline)

(write
  (let* ((x 3)
         (x (* x x))
         (y 4)
         (y (* y y))
         (z (let ((x 3) (y 4)) (+ (* x x) (* y y)))))
    (- z (+ x y))))
(newline)
