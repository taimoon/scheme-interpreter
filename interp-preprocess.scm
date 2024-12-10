(set! list (lambda x x))

(define-macro (define header . es)
  (if (pair? header)
      (list 'set!
        (car header)
        (list 'lambda (cdr header) (cons 'begin es)))
      (list 'set! header (car es))))

;;;; cxr
(define caar (lambda (x) (car (car x))))
(define caaar (lambda (x) (car (car (car x)))))
(define caaaar (lambda (x) (car (car (car (car x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cdar (lambda (x) (cdr (car x))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cadr (lambda (x) (car (cdr x))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

; workaround to make map function private
(define-macro (let bs . es) ((lambda (map)
  (set! map (lambda (f xs)
    (if (pair? xs)
        (cons (f (car xs)) (map f (cdr xs)))
        '())))
  (if (pair? bs)
      (cons (list 'lambda (map car bs) (cons 'begin es))
            (map (lambda (b) (cadr b)) bs))
      (if (symbol? bs)
          (let ((name bs)
                (inits (map cadr (car es)))
                (params (map car (car es)))
                (body (cdr es)))
            (list 'letrec
              (list
                (list name (list 'lambda params (cons 'begin body))))
              (cons name inits)))
          (cons 'begin es)))
  ) 0))

(define-macro (let* bindings . body)
  (define (let*->let-aux bindings body)
    (if (null? (cdr bindings))
        (list 'let (list (car bindings)) (cons 'begin body))
        (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))
  (if (null? bindings)
      (cons 'begin body)
      (let*->let-aux bindings body)))

(define-macro (cond . clauses)
  (if (not (pair? clauses))
      #f
      (let ((pred (caar clauses))
            (conseq (cons 'begin (cdar clauses)))
            (clauses (cdr clauses)))
        (if (eq? pred 'else)
            (if (pair? clauses)
                (error "cond-clauses->ifs" "misplaced else" clauses)
                conseq)
            (list 'if pred
                      conseq
                      (cons 'cond clauses))))))

(define-macro (and . es)
  (if (pair? es)
      (list 'if (car es) (cons 'and (cdr es)) #f)
      #t))

(define-macro (or . es)
  (if (pair? es)
      (list 'if (car es) #t (cons 'or (cdr es)))
      #f))

(define-macro (not e)
  (list 'if e #f #t))

(define (error . es)
  (write "error") (newline)
  (write es) (newline)
  (exit 1))

(define (fold-right proc init xs)
  (if (pair? xs)
      (proc (car xs) (fold-right proc init (cdr xs)))
      init))

(define (improper-list? xs)
  (cond
    ((null? xs) #f)
    ((pair? xs) (improper-list? (cdr xs)))
    (else #t)))

(define (improper->proper xs)
  (cond
    ((null? xs) xs)
    ((symbol? xs) (list xs))
    ((pair? xs) (cons (car xs) (improper->proper (cdr xs))))
    (else (error "improper->proper" "unknown object" xs))))

(define (length xs)
  (fold-right (lambda (_ acm) (+ 1 acm)) 0 xs))

(let ((map 0)
      (fold-right 0)
      (append 0))

(define (map f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map f (cdr xs)))
      '()))

(define (fold-right proc init xs)
  (if (pair? xs)
      (proc (car xs) (fold-right proc init (cdr xs)))
      init))

(define (append xs ys)
  (fold-right cons ys xs))

(define-macro (letrec bs . es)
  (if (pair? bs)
      (append
        (list 'let (map (lambda (v) (list v 0)) (map car bs))) (append
        (map (lambda (var-init) (list 'set! (car var-init) (cadr var-init)))
              bs)
        es))
      (cons 'begin es)))
)

(load "lib/match-defmacro.scm")
(define-macro (match . e)
  (compile-match (cons 'match e)))

(define-macro (quasiquote e)
  (expand-qq e))

(define (make-env) '())

(define (extend-env xs vs env)
  (cond
    ((symbol? xs)
     (cons (list xs vs) env))
    ((pair? xs)
     (if (pair? vs)
         (cons (list (car xs) (car vs))
               (extend-env (cdr xs) (cdr vs) env))
         (error "extend-env" "bad value list" vs)))
    (else env)))

(define (maybe-apply-env x env)
  (cond
    ((null? env) #f)
    ((not (and (pair? env) (pair? (car env))))
     (error "maybe-apply-env" "improperly formed env"))
    ((eq? x (caar env))
     (car env))
    (else (maybe-apply-env x (cdr env)))))

(define (apply-env x env)
  (let ((res (maybe-apply-env x env)))
    (if res
        (cadr res)
        (error "apply-env" "unbound" x (map car env)))))

;;; uniquify
(define (init-uenv)
  (extend-env (map car uniquify-methods) (map cadr uniquify-methods) (make-env)))

;; sugars
(define (let*->let e)
  (if (null? (cadr e))
      (cons 'begin (cddr e))
      (let*->let-aux (cadr e) (cddr e))))

(define (let*->let-aux bindings body)
  (if (null? (cdr bindings))
      (list* 'let (list (car bindings)) body)
      (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))

(define (cond->ifs e)
  (if (or (not (pair? e)) (null? (cdr e)))
      (error "cond" "syntax error" e)
      (cond-clauses->ifs (cdr e))))

(define (cond-clauses->ifs clauses)
  (if (not (pair? clauses))
      #f
      (let ((pred (caar clauses))
            (conseq (cons 'begin (cdar clauses)))
            (clauses (cdr clauses)))
        (if (eq? pred 'else)
            (if (pair? clauses)
                (error "cond-clauses->ifs" "misplaced else" clauses)
                conseq)
            (list 'if pred
                      conseq
                      (cond-clauses->ifs clauses))))))

(define (not->if e)
  (list 'if e #f #t))

(define (or->ifs es)
  (if (pair? es)
      (list 'if (car es) #t (or->ifs (cdr es)))
      #f))

(define (and->ifs es)
  (if (pair? es)
      (list 'if (car es) (and->ifs (cdr es)) #f)
      #t))

(define (quote->cons q)
  (cond
    ((null? q) ''())
    ((pair? q)
     `(cons ,(quote->cons (car q))
            ,(quote->cons (cdr q))))
    ((symbol? q) `(quote ,q))
    (else q)))

(define (expand-qq form)
  (cond 
    ((not (pair? form)) (list 'quote form))
    ((eq? 'quasiquote (car form)) (expand-qq (cadr form)))
    ((eq? 'unquote (car form)) (cadr form))
    (else (qq-list form))))

(define (tail-unquote? form)
  (and
    (pair? form)
    (pair? (cdr form))
    (null? (cddr form))
    (eq? (car form) 'unquote)))

(define (tail-unquote a)
  (cadr a))

(define (qq-list form)
  (cond 
    ((null? form) ''())
    ((not (pair? form))
     (list 'list (expand-qq form)))
    ((tail-unquote? form)
     (tail-unquote form))
    ((and (pair? (car form))
          (eq? 'unquote-splicing (caar form)))
      (list 'append (cadar form) (qq-list (cdr form))))
    (else (list 'append (list 'list (expand-qq (car form))) (qq-list (cdr form))))))

(define (linearize-seq es)
  (define (linearize e)
    (if (and (pair? e) (eq? (car es) 'begin))
        (map linearize-seq (cdr es))
        (list e)))
  (fold-right (lambda (e acm) (append (linearize e) acm)) '() es))

(define (linearize-begin es)
  (let ((es (linearize-seq es)))
    (if (= (length es) 1)
        (car es)
        (cons 'begin es))))

(define (uniquify-each es env)
  (if (pair? es)
      (cons (uniquify (car es) env)
            (uniquify-each (cdr es) env))
      '()))

(define (uniquify-seq es env)
  (collect-definition (linearize-seq es)
    (lambda (defns es) 
      (define (defn->binding d)
        (match d
          ((define (,fn . ,params) . ,body)
           `(,fn (lambda ,params . ,body)))
          ((define ,x ,e)
           `(,x ,e))
          (,() (error "defn->binding" "unmatch" d))))
      (cond
        ((pair? defns)
         (list (uniquify `(letrec ,(map defn->binding defns) . ,es) env)))
        (else (uniquify-each es env))))))

(define uniquify-methods (list
  (list 'quote
    (lambda (e env) e))
  (list 'quasiquote
    (lambda (e env) (uniquify (expand-qq (cadr e)) env)))
  (list 'begin
    (lambda (e env)
      (let ((e* (uniquify-seq (cdr e) env)))
        (if (and (pair? e*) (not (pair? (cdr e*))))
            (car e*)
            (cons 'begin e*)))))
  (list 'set!
    (lambda (e env)
      `(set! ,(cadr e) ,(uniquify (caddr e) env))))
  (list 'if
    (lambda (e env)
      (let ((pred (cadr e))
            (conseq (caddr e))
            (altern (if (pair? (cdddr e)) (cadddr e) #f)))
      `(if ,(uniquify pred env) 
           ,(uniquify conseq env) 
           ,(uniquify altern env)))))
  (list 'cond
    (lambda (e env)
      (uniquify (cond-clauses->ifs (cdr e)) env)))
  (list 'not
    (lambda (e env) (uniquify (not->if (cadr e)) env)))
  (list 'and
    (lambda (e env) (uniquify (and->ifs (cdr e)) env)))
  (list 'or
    (lambda (e env) (uniquify (or->ifs (cdr e)) env)))
  (list 'match
    (lambda (e env) (uniquify (compile-match e) env)))
  (list 'case-lambda
    (lambda (e env)
     (uniquify
      `(lambda args
        (apply
          (cond
            ,@(map (lambda (clause)
                    `((,(if (improper-list? (car clause)) '<= '=)
                       ,(length (improper->proper (car clause)))
                       (length args))
                       (lambda ,@clause)))
                  (cdr e))
            (else (error args)))
          args))
      env)))
  (list 'lambda
    (lambda (e env)
      (let* ((params (cadr e))
             (body (cddr e))
             (params* (improper->proper params))
             (env (extend-env params* params* env)))
        `(lambda ,params . ,(uniquify-seq body env)))))
  (list 'let
    (lambda (e env)
      (let ((bs (cadr e))
            (es (cddr e)))
        (cond
          ((null? bs)
           (uniquify (cons 'begin es) env))
          ((symbol? bs)
           (let* ((fn bs)
                  (inits (car es))
                  (es (cdr es))
                  (params (map car inits))
                  (args (map cadr inits)))
            (uniquify
              `(letrec ((,fn (lambda ,params . ,es)))
                (,fn . ,args))
              env)))
          (else
            (let* ((xs (map car bs))
                   (inner-env (extend-env xs xs env))
                   (inits (map (lambda (b) (uniquify (cadr b) env)) bs)))
              `((lambda ,xs ,@(uniquify-seq es inner-env)) ,@inits)))))))
  (list 'let*
    (lambda (e env)
      (uniquify (let*->let e) env)))
  (list 'letrec
    (lambda (e env)
      (let ((bs (cadr e))
            (es (cddr e)))
        (define (simplify-letrec bs es)
          (let* ((vars (map car bs))
                 (inits (map cadr bs)))
            ;;; unfaithful implementation
            `(let ,(map (lambda (v) (list v 0)) vars)
                ,@(map (lambda (v init) `(set! ,v ,init)) vars inits)
                ,@es)))
        (uniquify (simplify-letrec bs es) env))))
  (list 'define-macro
    (lambda (e env)
      (let* ((header (cadr e))
             (body (cddr e))
             (env (extend-env (list (car header)) (list (car header)) env)))
        `(define-macro ,header . ,(uniquify-seq body env)))
      ))
  (list 'include (lambda _ ''()))
  (list 'import (lambda _ ''()))
  ))

(define (uniquify e env)
  (if (not (pair? e))
      e
      (let ((res (maybe-apply-env (car e) env)))
        (if (not (and res (procedure? (cadr res))))
            (uniquify-each e env)
            ((cadr res) e env)))))

(define (definition-variable defn)
  (match defn
    ((define (,fn . ,()) . ,()) fn)
    ((define ,fn ,()) fn)
    (,() (error "definition-variable" "unmatch" defn))))

(define (definition-value defn)
  (match defn
    ((define (,() . ,params) . ,body)
     `(lambda ,params  . ,body))
    ((define ,() ,e)
     e)
    (,() (error "definition-value" "unmatch" defn))))

(define (collect-definition es cont)
  (if (pair? es)
      (match (car es)
        ((define . ,())
          (collect-definition (cdr es)
           (lambda (defns es*)
             (cont (cons (car es) defns) es*))))
        (,e (collect-definition (cdr es)
              (lambda (defns es) (cont defns (cons e es))))))
      (cont '() '())))

(define (uniquify-program prog)
  (match prog
    ((begin . ,es)
     (collect-definition es
      (lambda (defns es)
        (let* ((vars (map definition-variable defns))
               (vals (map definition-value defns))
               (env (extend-env vars vars (init-uenv)))
               (vals* (uniquify-each vals env))
               (es (uniquify-each es env)))
          `(begin
            ,@(map (lambda (v w) `(set! ,v ,w)) vars vals*)
            ,@es)))))
    (,() (error "uniquify-program" "unmatch" prog))))
