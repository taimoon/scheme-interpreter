(define (map f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map f (cdr xs)))
      '()))

(define list (lambda x x))

(define cadr (lambda (x) (car (cdr x))))

(define (and->ifs es)
  (if (pair? es)
      ((lambda x x) 'if (car es) (and->ifs (cdr es)) #f)
      #t))

(defmacro (and . es)
  (and->ifs es))

(defmacro (let bs . es)
  (cons
    (list 'lambda (map car bs) (cons 'begin es))
    (map cadr bs)))

(write (make-vector 0))
(newline)

(write (vector? (make-vector 0)))
(newline)

(write (make-vector 3))
(newline)

(write (vector-length (make-vector 3)))
(newline)

(let ((xs (make-vector 3)))
  (vector-set! xs 0 17)
  (vector-set! xs 1 23)
  (vector-set! xs 2 29)
  (write xs)
  (newline)
  (write (vector-length xs))
  (newline)
  (write (vector? xs))
  (newline))

(let ((xs (make-vector 3)))
  (vector-set! xs 2 29)
  (write (vector-ref xs 2))
  (newline)
  (write (vector-length xs))
  (newline)
  (write (vector? xs))
  (newline))

(let ((xs (make-vector 3)))
  (vector-set! xs 0 2)
  (vector-set! xs 1 (vector-length xs))
  (vector-set! xs 2 (+ (vector-ref xs 0) (vector-ref xs 1)))
  (write xs)
  (newline)
  (write (vector-length xs))
  (newline)
  (write (vector? xs))
  (newline))

(write (make-vector 3 0)) (newline)
(write (make-vector 5 #t)) (newline)
(let ((v (make-vector 7 (make-vector 0))))
  (write
    (and (eq? (vector-ref v 0) (vector-ref v 6))
         (eq? (vector-ref v 3) (vector-ref v 5))))
  (newline))
