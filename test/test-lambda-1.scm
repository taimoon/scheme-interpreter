(let ()
  (define (partition-k pred xs k)
    (if (null? xs)
        (k '() '())
        (let ()
          (define (k* l r)
            (if (pred (car xs))
                (k (cons (car xs) l) r)
                (k l (cons (car xs) r))))
          (partition-k pred (cdr xs) k*))))
  (define (even? n)
    (eq? (mod n 2) 0))
  
  (partition-k even? '(0 1 2 3 4 5)
    (lambda (evens odds)
      (write evens) (newline)
      (write odds) (newline)))
)