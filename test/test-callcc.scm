(define writeln (lambda (x) (write x) (newline)))
(define list (lambda x x))
(define even? (lambda (n) (if (<= n 0) #t (odd? (- n 1)))))
(define odd? (lambda (n) (if (<= n 0) #f (even? (- n 1)))))
(writeln (apply + '(2 3)))
(define values (lambda vs (call/cc (lambda (k) (apply k vs)))))
(define (partition pred xs)
  (if (pair? xs)
      (call-with-values
        (lambda () (partition pred (cdr xs)))
        (lambda (ys zs)
          (if (pred (car xs))
              (values (cons (car xs) ys) zs)
              (values ys (cons (car xs) zs)))))
      (values '() '())))

(writeln (call/cc (lambda (k) 0)))
(writeln (call/cc (lambda (k) (k 0))))
(writeln (call/cc (lambda (k) (+ #\1 (k 0)))))
(writeln
(call-with-values
  (lambda () (values 2 3))
  list)
)
(writeln
(call-with-values
  (lambda () (values))
  list)
)
(writeln
  (call-with-values (lambda () (partition even? '(1 2 3 4 5))) list))