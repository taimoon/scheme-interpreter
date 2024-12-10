(define (match-expression? e) (and (pair? e) (eq? (car e) 'match)))
(define (match-input-expr e) (cadr e))
(define (match-clauses e) (cddr e))
(define (first-clause cs) (car cs))
(define (guard-clause? c)
  (and (pair? (cadr c)) (eq? (car (cadr c)) 'guard)))
(define (guard-clause-expr c)
  (if (guard-clause? c)
      (cadr (cadr c))
      #t))
(define (rest-clause cs) (cdr cs))
(define (clause-pattern c) (car c))
(define (clause-expr c)
  (if (guard-clause? c)
      (cddr c)
      (cdr c)))
(define (unquoted-empty? e)
  (and (pair? e) (eq? (car e) 'unquote)
       (pair? (cdr e)) (eq? (cadr e) '())))
(define (empty-pat? e) (unquoted-empty? e))
(define (var-pat? e) 
  (and (pair? e) (eq? (car e) 'unquote)
       (pair? (cdr e)) (symbol? (cadr e))))
(define (var-pat e) (cadr e))
(define (datum-pattern? pattern)
  (or (null? pattern) (symbol? pattern) (not (pair? pattern))))
(define (datum-pattern pattern)
  (if (or (null? pattern) (symbol? pattern))
      `(quote ,pattern)
      pattern))
(define (compile-match p)
  (let ((evaluated (gensym)))
    `(let ((,evaluated ,(match-input-expr p)))
      ,(compile-clauses evaluated (match-clauses p)))))
(define (compile-clauses value clauses)
  (if (null? clauses)
      `'(error 'no-matching-pattern ,value)
      (compile-clause
        (list (list (clause-pattern (first-clause clauses)) value))
        '(and)
        '()
        (clause-expr (first-clause clauses))
        (compile-clauses value (rest-clause clauses))
        (guard-clause-expr (first-clause clauses)))))

(define (compile-clause pat-val-pairs condition bindings conseq alter guard-pred)
  (cond
    ((and (null? pat-val-pairs) (eq? guard-pred #t))
     `(if ,condition (let ,bindings . ,conseq) ,alter))
    ((null? pat-val-pairs)
     (let ((alter-f (gensym)))
       `(let ((,alter-f (lambda () ,alter)))
         (if ,condition
             (let ,bindings 
               (if ,guard-pred 
                   (begin . ,conseq)
                   (,alter-f)))
             (,alter-f)))))
    ((empty-pat? (caar pat-val-pairs))
     (compile-clause (cdr pat-val-pairs)
                     condition
                     bindings
                     conseq
                     alter
                     guard-pred))
    ((var-pat? (caar pat-val-pairs))
     (compile-clause (cdr pat-val-pairs)
                     condition
                     (cons (list (var-pat (caar pat-val-pairs)) (cadar pat-val-pairs)) bindings)
                     conseq
                     alter
                     guard-pred))
    ((datum-pattern? (caar pat-val-pairs))
     (compile-clause (cdr pat-val-pairs)
                     (append condition 
                             `((equal? ,(datum-pattern (caar pat-val-pairs)) ,(cadar pat-val-pairs))))
                     bindings
                     conseq
                     alter
                     guard-pred))
    ((pair? (caar pat-val-pairs))
     (let* ((first (car pat-val-pairs))
            (rest (cdr pat-val-pairs))
            (pat (car first))
            (val (cadr first))
            (left (list (car pat) (list 'car val)))
            (right (list (cdr pat) (list 'cdr val))))
      (compile-clause (append (list left right) rest)
                      (append condition `((pair? ,val)))
                      bindings
                      conseq
                      alter
                      guard-pred)))
    (else (error compile-clause '()))))
