(let ((k (cons 2 3))
      (v (cons 5 7)))
  (let ((e (make-ephemeron k v)))
    (writeln
      (and (ephemeron? e)
           (eq? (ephemeron-key e) k)
           (eq? (ephemeron-value e) v)))))
