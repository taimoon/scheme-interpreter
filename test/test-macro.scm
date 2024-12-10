(set! list (lambda x x))

(define-macro (define header . es)
  (if (pair? header)
      (list 'set!
        (car header)
        (list 'lambda (cdr header) (cons 'begin es)))
      (list 'set! header (car es))))

(define (map f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map f (cdr xs)))
      '()))

(define-macro (let bs . es)
  (if (pair? bs)
      (cons (list 'lambda (map car bs) (cons 'begin es))
            (map (lambda (b) (car (cdr b))) bs))
      (cons 'begin es)))

(let ()
  (writeln (+ 2 3)))

(let ((x 3)
      (y 4)
      (sqr (lambda (x) (* x x))))
  (writeln (+ (sqr x) (sqr y))))

