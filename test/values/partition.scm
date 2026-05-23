(let ()
(define (partition pred xs)
  (if (pair? xs)
      (call-with-values
        (lambda () (partition pred (cdr xs)))
        (lambda (truthy falsy)
          (if (pred (car xs))
              (values (cons (car xs) truthy) falsy)
              (values truthy (cons (car xs) falsy)))))
      (values '() '())))

(define (even? n) (= (mod n 2) 0))

(call-with-values
    (lambda () (partition even? '(1 2 3 4 5)))
    (lambda vs (write vs) (newline)))
)