(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))
(define (max x y) (if (< x y) y x))
(define (min x y) (if (< x y) x y))

;;;; list
(define (fold-left proc init xs)
  (if (pair? xs)
      (fold-left proc (proc init (car xs)) (cdr xs))
      init))

(define (fold-right proc init xs)
  (if (pair? xs)
      (proc (car xs) (fold-right proc init (cdr xs)))
      init))

(define (append . xss)
  (if (null? xss)
      '()
      (let recur ((xs (car xss)) (xss (cdr xss)))
        (if (null? xss)
            xs
            (fold-right cons (recur (car xss) (cdr xss)) xs)))))

(define (reverse xs)
  (fold-left (lambda (acm x) (cons x acm)) '() xs))

(define (length xs)
  (fold-left (lambda (acm _) (add1 acm)) 0 xs))

(define (map f ls . more)
  (if (null? more)
      (let map1 ((ls ls))
        (if (null? ls)
            '()
            (cons (f (car ls))
                  (map1 (cdr ls)))))
      (let map-more ((ls ls) (more more))
        (if (null? ls)
            '()
            (cons
              (apply f (cons (car ls) (map car more)))
              (map-more (cdr ls) (map cdr more)))))))

(define (filter pred xs)
  (cond
    ((not (pair? xs)) '())
    ((pred (car xs))
     (cons (car xs) (filter pred (cdr xs))))
    (else
     (filter pred (cdr xs)))))

(define (for-each f xs . xss)
  (if (null? xss)
      (let loop ((xs xs))
        (if (pair? xs)
            (begin  (f (car xs))
                    (loop (cdr xs)))
            '()))
      (let loop ((xs xs) (xss xss))
        (if (null? xs)
            '()
            (begin
              (apply f (cons (car xs) (map car xss)))
              (loop (cdr xs) (map cdr xss)))))))
