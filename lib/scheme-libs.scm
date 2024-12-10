(define (sub1 x) (- x 1))
(define (add1 x) (+ x 1))

(define (list . x) x)

(define (cons* . xs)
  (cond
    ((null? xs) xs)
    ((and (pair? xs) (null? (cdr xs)))
     (car xs))
    (else
      (let recur ((xs xs))
        (if (and (pair? xs) (null? (cdr xs)))
            (car xs)
            (cons (car xs) (recur (cdr xs))))))))

(define list* cons*)

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

(define (add1 x) (+ x 1))

(define (length xs)
  (fold-left (lambda (acm _) (add1 acm)) 0 xs))

;;; vector
(define (vector . xs)
  (list->vector xs))

(define (list->vector xs)
  (let recur ((xs xs) (sz 0))
    (if (pair? xs)
        (let ((v (recur (cdr xs) (add1 sz))))
          (vector-set! v sz (car xs))
          v)
        (make-vector sz))))

;;; misc
(define (make-lcg multiplier increment modulus x)
  (lambda ()
    (set! x (mod (+ increment (* multiplier x)) modulus))
    x))

(define (random-bool)
  (= (mod (rand) 2) (mod (rand) 2)))

(define (random-string len)
  (let loop ((s (make-string len))
             (i (- len 1)))
    (if (< i 0)
        s
        (begin
          (string-set! s i (integer->char
                            (+ (if (random-bool) 97 65)
                               (mod (rand) 26))))
          (loop s (- i 1))))))

(define rand
  (make-lcg 75 74 (+ (ash 2 16) 1) (get-process-id)))

(define (gensym) (string->symbol (random-string 16)))

(define (equal? x y)
  (cond ((eq? x y) #t)
        ((and (pair? x) (pair? y))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((and (vector? x) (vector? y))
         (vector-equal? x y))
        ((and (string? x) (string? y))
         (string=? x y))
        (else #f)))

(define (string=? s1 s2)
  (or (eq? s1 s2)
      (and (string? s1) (string? s2)
           (= (string-length s1) (string-length s2))
           (let loop ((i 0))
             (cond
               ((eq? i (string-length s1))
                 #t)
               ((eq? (string-ref s1 i) (string-ref s2 i))
                 (loop (add1 i)))
               (else #f))))))

(define (string-copy! src src-start dst dst-start n)
  (if (= n 0)
      dst
      (begin
        (string-set! dst dst-start (string-ref src src-start))
        (string-copy! src (add1 src-start) dst (add1 dst-start) (sub1 n)))))

(define (string-append . ss)
  (let ((res (make-string (fold-left + 0 (map string-length ss)))))
    (let loop ((ss ss) (split 0))
      (if (pair? ss)
          (begin
            (string-copy! (car ss) 0 res split (string-length (car ss)))
            (loop (cdr ss) (+ (string-length (car ss)) split)))  
          res))))