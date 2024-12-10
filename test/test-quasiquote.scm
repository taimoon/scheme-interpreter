(let ()
(define (fold-left proc init xs)
  (if (pair? xs)
      (fold-left proc (proc init (car xs)) (cdr xs))
      init))

(define (fold-right proc init xs)
  (if (pair? xs)
      (proc (car xs) (fold-right proc init (cdr xs)))
      init))

(define (append xs ys)
  (fold-right cons ys xs))

(define (write-line x)
  (write x)
  (newline))

(write-line
  (let ((fn 'cons*) (params 'xs))
    (list (append (list fn) params))))

(write-line
  (let ((fn 'cons*) (params 'xs))
    (append
      (list 'define)
      (append (list (append (list fn) params)) '()))))

(write-line
  (let ((fn 'cons*)
        (params 'xs))
    `(define (,fn ,params))))

(write-line
  (let ((fn 'cons*)
        (params 'xs))
    `(define (,fn ,params))
    `(define (,fn . ,params))))
)