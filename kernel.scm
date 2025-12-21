(define uniquify-methods '())

(define (uniquify-methods-add! kw p)
  (set! uniquify-methods
        (cons (cons kw p) uniquify-methods)))

(define (uniquify-methods-add-macro! kw p)
  (set! uniquify-methods
        (cons (cons kw (cons 'macro p)) uniquify-methods)))

(define install-macros-prog '(lambda ()
  (define list (lambda x x))
  (define caar (lambda (x) (car (car x))))
  (define cadr (lambda (p) (car (cdr p))))
  (define cdar (lambda (x) (cdr (car x))))
  (define cddr (lambda (x) (cdr (cdr x))))
  (define cadar (lambda (x) (car (cdr (car x)))))
  (define null? (lambda (x) (eq? x '())))
  (define (not e) (eq? e #f))
  (define (length xs) (if (pair? xs) (+ 1 (length (cdr xs))) 0))
  (define (map f xs)
    (if (pair? xs)
        (cons (f (car xs)) (map f (cdr xs)))
        '()))
  (define (append xs ys)
    (if (pair? xs)
        (cons (car xs) (append (cdr xs) ys))
        ys))
  (define (make-begin es)
    (if (pair? (cdr es))
        (cons 'begin es)
        (car es)))
  (define (let->lambda bs e)
    (cons
      (list 'lambda (map car bs) e)
      (map cadr bs)))
  (define (named-let->letrec fn bs es)
    (list 'letrec (list (list fn (list 'lambda (map car bs) (make-begin es))))
      (cons fn (map cadr bs))))
  (defmacro (define-macro var . es)
    (if (symbol? var)
        (list
          (list 'lambda '(proc)
            (list 'uniquify-methods-add-macro! (list 'quote var) 'proc)
            (list 'defmacro var 'proc))
          (car es))
        (list 'define-macro (car var) (list 'lambda (cdr var) (make-begin es)))))
  (define-macro (define-macro var . es)
    (if (symbol? var)
        (list
          (list 'lambda '(proc)
            (list 'uniquify-methods-add-macro! (list 'quote var) 'proc)
            (list 'defmacro var 'proc))
          (car es))
        (list 'define-macro (car var) (list 'lambda (cdr var) (make-begin es)))))
  (define-macro (let bs . es)
    (if (symbol? bs)
        (named-let->letrec bs (car es) (cdr es))
        (let->lambda bs (make-begin es))))
  (define-macro (letrec bs . es)
    (cons
      (list 'lambda (map car bs)
        (make-begin
          (append
            (map (lambda (e) (cons 'set! e)) bs)
            es)))
      (map (lambda _ 0) bs)))
  (define (cond-clauses->ifs clauses)
    (if (pair? clauses)
        (let ((pred (caar clauses))
              (conseq (make-begin (cdar clauses)))
              (clauses (cdr clauses)))
          (if (eq? pred 'else)
              (if (pair? clauses)
                  (error "cond-clauses->ifs" "misplaced else" clauses)
                  conseq)
              (if (pair? clauses)
                  (list 'if pred
                            conseq
                            (cond-clauses->ifs clauses))
                  (list 'if pred conseq))))
        #f))
  (define-macro (cond clause . clauses)
    (cond-clauses->ifs (cons clause clauses)))

  (define (let*->let-aux bindings body)
    (if (null? (cdr bindings))
        (list 'let (list (car bindings)) body)
        (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))
  
  (define-macro (let* bs . es)
    (let*->let-aux bs (make-begin es)))

  (define-macro (and . es)
    (if (pair? es)
        (list 'if (car es) (cons 'and (cdr es)) #f)
        #t))

  (define-macro (or . es)
    (if (pair? es)
        (list 'if (car es) #t (cons 'or (cdr es)))
        #f))
  
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
  
  (define-macro (quasiquote e)
    (expand-qq e))
  
  (define (improper-list? e)
    (if (pair? e)
        (improper-list? (cdr e))
        (not (null? e))))

  (define (case-lambda->lambdas cs)
    (define (case-lambda-clause->clause argc args c)
      (define params (car c))
      (define body (cons 'begin (cdr c)))
      `(,(if (improper-list? params)
             (list '<= (length params) argc)
             (list '= (length params) argc))
        (apply (lambda ,params ,body) ,args)))
    `(let ()
      (define (length xs) (if (pair? xs) (+ 1 (length (cdr xs))) 0))
      (lambda args
        (let ((argc (length args)))
          (cond
            ,@(map (lambda (c) (case-lambda-clause->clause 'argc 'args c)) cs)
            (error "case-lambda" "wrong-argument-number"))))))

  (define-macro (case-lambda clause . clauses)
    (if (pair? clauses)
        (case-lambda->lambdas (cons clause clauses))
        (cons 'lambda clause)))
))

((eval install-macros-prog))

(define apply
  (let ((apply apply))
    (define (cons* x xs)
      (if (pair? xs)
          (cons x (cons* (car xs) (cdr xs)))
          x))
    (lambda (fn v . vs)
      (apply fn (cons* v vs)))))

(define values (lambda vs (call/cc (lambda (k) (apply k vs)))))

(define (list->vector xs)
  (let recur ((i 0) (xs xs))
    (if (pair? xs)
        (let ((v (recur (+ i 1) (cdr xs))))
          (vector-set! v i (car xs))
          v)
        (make-vector i))))

(define (vector . xs) (list->vector xs))

(define (%utf32->utf8! c32 buf off)
  (let ((c32 (char->integer c32)))
    (cond
      ((<= c32 #x7F)
        (bytevector-u8-set! buf (+ off 0) c32)
        1)
      ((<= c32 #x7FF)
        (bytevector-u8-set! buf (+ off 0) (bitwise-ior #xC0 (ash c32 -6)))
        (bytevector-u8-set! buf (+ off 1) (bitwise-ior #x80 (bitwise-and c32 #x3F)))
        2)
      ((and (<= c32 #xFFFF)
            (>= c32 #xD800)
            (<= c32 #xDFFF))
        0)
      ((<= c32 #xFFFF)
        (bytevector-u8-set! buf (+ off 0) (bitwise-ior #xE0 (ash c32 -12)))
        (bytevector-u8-set! buf (+ off 1) (bitwise-ior #x80 (bitwise-and (ash c32 -6) #x3F)))
        (bytevector-u8-set! buf (+ off 2) (bitwise-ior #x80 (bitwise-and c32 #x3F)))
        3)
      ((<= c32 #x10FFFF)
        (bytevector-u8-set! buf (+ off 0) (bitwise-ior #xF0 (ash c32 -18)))
        (bytevector-u8-set! buf (+ off 1) (bitwise-ior #x80 (bitwise-and (ash c32 -12) #x3F)))
        (bytevector-u8-set! buf (+ off 2) (bitwise-ior #x80 (bitwise-and (ash c32 -6) #x3F)))
        (bytevector-u8-set! buf (+ off 3) (bitwise-ior #x80 (bitwise-and c32 #x3F)))
        4)
      (else 0))))

(define (char-utf-8-length c)
  (let ((c32 (char->integer c)))
    (cond
      ((<= c32 #x7F) 1)
      ((<= c32 #x7FF) 2)
      ((and (<= c32 #xFFFF)
            (>= c32 #xD800)
            (<= c32 #xDFFF))
       0)
      ((<= c32 #xFFFF) 3)
      ((<= c32 #x10FFFF) 4)
      (else 0))))

(define %string->utf8 (let ()
  (define (string-fold f init s)
    (let loop ((i 0)
               (init init))
        (if (>= i (string-length s))
            init
            (loop (+ i 1) (f init (string-ref s i))))))

  (define (string-utf8-length s)
    (string-fold (lambda (s c) (+ s (char-utf-8-length c))) 0 s))

  (define (%string->utf8 s null-term?)
    (let loop ((i 0)
               (j 0)
               (buf (make-bytevector (+ (string-utf8-length s) (if null-term? 1 0)) 0)))
      (if (>= i (string-length s))
          buf
          (loop (+ i 1)
                (+ j (%utf32->utf8! (string-ref s i) buf j))
                buf))))
  %string->utf8))

(define (string->utf8 s)
  (%string->utf8 s #f))

(define (list->bytevector vs)
  (let recur ((vs vs) (i 0))
    (if (pair? vs)
        (let ((bv (recur (cdr vs) (+ i 1))))
          (bytevector-u8-set! bv i (car vs))
          bv)
        (make-bytevector i))))

(define (bytevector . vs) (list->bytevector vs))

(define (bytevector->list bv)
  (let recur ((i 0))
    (if (< i (bytevector-length bv))
        (cons (bytevector-u8-ref bv i) (recur (add1 i)))
        '())))

(define (%make-foreign-procedure fn argc)
  (vector-ref
    (vector
      (lambda () (foreign-call fn (vector)))
      (lambda (v0) (foreign-call fn (vector v0)))
      (lambda (v0 v1) (foreign-call fn (vector v0 v1)))
      (lambda (v0 v1 v2) (foreign-call fn (vector v0 v1 v2)))
      (lambda (v0 v1 v2 v3) (foreign-call fn (vector v0 v1 v2 v3)))
      (lambda (v0 v1 v2 v3 v4) (foreign-call fn (vector v0 v1 v2 v3 v4)))
      (lambda (v0 v1 v2 v3 v4 v5) (foreign-call fn (vector v0 v1 v2 v3 v4 v5))))
    argc))

(define-macro (make-foreign-procedure fn argc)
  ((lambda x x) '%make-foreign-procedure (%string->utf8 (symbol->string fn) #t) argc))

(define abort (make-foreign-procedure abort 0))

(define error
  (let ((write write)
        (newline newline))
    (lambda x
      (write "error: " stderr)
      (write x stderr)
      (newline stderr)
      (abort))))

(define (not x) (if x #f #t))
(define (null? x) (eq? x '()))
(define (append xs ys)
  (if (pair? xs)
      (cons (car xs) (append (cdr xs) ys))
      ys))
(define (list . x) x)
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

(define gensym
  (let ()
    (define (abs x) (if (< x 0) (- x) x))

    (define (make-lcg multiplier increment modulus x)
      (lambda ()
        (set! x (mod (+ increment (* multiplier x)) modulus))
        x))

    (define rand
      (make-lcg 75 74 (+ (ash 2 16) 1) 0))

    (define (random-string len)
      (let loop ((s (make-string len))
                 (i (- len 1)))
        (if (< i 0)
            s
            (begin
              (string-set! s i (integer->char (+ 97 (mod (rand) 26))))
              (loop s (- i 1))))))
    (define (int->char x)
      (integer->char (+ x (char->integer #\0))))
    (define (string-copy! src src-start dst dst-start n)
      (if (= n 0)
          dst
          (begin
            (string-set! dst dst-start (string-ref src src-start))
            (string-copy! src (+ 1 src-start) dst (+ 1 dst-start) (- n 1)))))
    (define (string-append s1 s2)
      (let ((s (make-string (+ (string-length s1) (string-length s2)))))
        (string-copy! s1 0 s 0 (string-length s1))
        (string-copy! s2 0 s (string-length s1) (string-length s2))
        s))
    (define (number->string x)
      (let recur ((x (abs x))
                  (i 0))
        (cond
          ((> x 0)
            (let ((s (recur (div x 10) (+ 1 i))))
              (string-set! s (- (- (string-length s) 1) i) (int->char (mod x 10)))
              s))
          ((= i 0) "0")
          (else (make-string i #\0)))))
    (define counter 0)
    (define (*->str sym) (if (symbol? sym) (symbol->string sym) sym))
    (define rdm-str (string-append "-" (string-append (random-string 4) "-")))
    (define %gensym
      (case-lambda
        (() (%gensym "g"))
        ((prefix)
          (set! counter (+ 1 counter))
          (string->symbol
            (string-append
              (string-append (*->str prefix) rdm-str)
              (number->string counter))))))
    %gensym))

(define (extend-env xs vs env)
  (cond
    ((null? xs) env)
    ((symbol? xs) (cons (list xs vs) env))
    ((not (pair? xs)) (error "extend-env" "ill-form" xs vs))
    (else
     (cons (list (car xs) (car vs)) (extend-env (cdr xs) (cdr vs) env)))))
(define assq (make-foreign-procedure s_assq 2))
(define (maybe-apply-env x env) (assq x env))
(define (make-env) '())
(define (make-begin es) (if (pair? (cdr es)) (cons 'begin es) (car es)))

(define (uniquify-each es env)
  (if (pair? es)
      (cons (uniquify (car es) env) (uniquify-each (cdr es) env))
      (if (null? es)
          '()
          (uniquify es env))))

(define uniquify-prog '
(define (uniquify e env)
  (cond
    ((symbol? e)
     (let ((r (maybe-apply-env e env)))
      (if r (cadr r) e)))
    ((not (pair? e)) e)
    ((symbol? (car e))
     (define r (maybe-apply-env (car e) env))
     (cond
      ((not (pair? r))
       (cons (car e) (uniquify-each (cdr e) env)))
      ((procedure? (cdr r))
       (apply (cdr r) env (cdr e)))
      ((and (pair? (cdr r)) (eq? (cadr r) 'macro) (procedure? (cddr r)))
       (uniquify (apply (cddr r) (cdr e)) env))
      (else (uniquify-each e env))))
    (else (uniquify-each e env))))
)

(define (uniquify* e)
  (uniquify e uniquify-methods))
(define (map* f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map* f (cdr xs)))
      (if (null? xs)
          '()
          (f xs))))
(uniquify-methods-add! 'quote
  (lambda (env e) (list 'quote e)))
(uniquify-methods-add! 'begin
  (lambda (env . es) (make-begin (uniquify-each es env))))
(uniquify-methods-add! 'lambda
  (lambda (env params . es)
    (let* ((params* (map* gensym params))
           (env (extend-env params params* env)))
      `(lambda ,params* ,(uniquify (make-begin es) env)))))
(uniquify-methods-add! 'set!
  (lambda (env var val) (list 'set! (uniquify var env) (uniquify val env))))
(uniquify-methods-add! 'define
  (lambda (env var . val)
    (if (symbol? var)
        (list 'define var (uniquify (car val) (extend-env var var env)))
        (uniquify (list 'define (car var) (list 'lambda (cdr var) (make-begin val))) env))))
(uniquify-methods-add! 'defmacro
  (lambda (env var . val)
    (if (symbol? var)
        (list 'defmacro var (uniquify (car val) (extend-env var var env)))
        (uniquify (list 'defmacro (car var) (list 'lambda (cdr var) (make-begin val))) env))))

(eval uniquify-prog)
(eval (uniquify* uniquify-prog))
((eval (uniquify* install-macros-prog)))

(define fwrite (make-foreign-procedure s_fwrite 4))

(define fread (make-foreign-procedure s_fread 4))

(define writeln
  (case-lambda
    ((x) (writeln x stdout))
    ((x op) (write x op) (newline op))))

(define fopen
  (let ((fopen (make-foreign-procedure s_fopen 2)))
    (lambda (path mode)
      (fopen (%string->utf8 path #t) (%string->utf8 mode #t)))))

(define fclose (make-foreign-procedure s_fclose 1))

(define getenv
  (let ((getenv (make-foreign-procedure s_getenv 1)))
    (lambda (var)
      (let ((r (getenv (%string->utf8 var #t))))
        (if r
            (utf8->string r)
            r)))))

(define setenv
  (let ((setenv (make-foreign-procedure s_setenv 2)))
    (lambda (var val)
      (setenv (%string->utf8 var #t) (%string->utf8 val #t)))))

(define eof-object (let ((eof eof)) (lambda () eof)))
(define eof-object? (let ((eof eof)) (lambda (v) (eq? eof v))))

(define (read-sexps-from-path path)
  (define ip (fopen path "r"))
  (define (recur e)
    (if (eof-object? e)
        '()
        (cons e (recur (read ip)))))
  (let ((v (recur (read ip))))
    (fclose ip)
    v))

(define eval (let ((eval eval)) (lambda (e) (eval (uniquify* e)))))

(define load
  (lambda (path)
    (eval (make-begin (map uniquify* (read-sexps-from-path path))))))

(define-macro (include path)
  (uniquify* (make-begin (read-sexps-from-path path))))

(define (vector-equal? v w)
  (cond
    ((eq? v w) #t)
    ((not (= (vector-length v) (vector-length w))) #f)
    (else
      (let loop ((i 0))
        (if (>= i (vector-length v))
            #t
            (and
              (equal? (vector-ref v i) (vector-ref w i))
              (loop (add1 i))))))))
(define (string=? s1 s2)
  (and
    (string? s1)
    (string? s2)
    (or
      (eq? s1 s2)
      (and
        (= (string-length s1) (string-length s2))
        (let loop ((i 0))
             (cond
               ((eq? i (string-length s1))
                 #t)
               ((eq? (string-ref s1 i) (string-ref s2 i))
                 (loop (add1 i)))
               (else #f)))))))
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

(include "lib/match-defmacro.scm")
(define-macro (match . e) (compile-match (cons 'match e)))
(include "lib/unicode.scm")
(define (string->list s)
  (let recur ((i 0))
    (if (< i (string-length s))
        (cons (string-ref s i) (recur (+ i 1)))
        '())))
(include "lib/scheme-libs.scm")
(include "lib/reader.scm")
(define (read-sexps-from-path path)
  (define ip (open-input-file path))
  (define (recur e)
    (if (eof-object? e)
        '()
        (cons e (recur (read ip)))))
  (let ((v (recur (read ip))))
    (close-port ip)
    v))
(define (writeln x) (write x) (newline))
(include "lib/writer.scm")

(define (abs x) (if (< x 0) (- x) x))
(define (max x y) (if (< x y) y x))
(define (min x y) (if (< x y) x y))

(define command-line (let ()
  (define (vector->list vs)
    (let iter ((i 0))
      (if (< i (vector-length vs))
          (cons (vector-ref vs i) (iter (+ i 1)))
          '())))
  (define args (map utf8->string (vector->list ARGS)))
  (lambda () args)
))

(define system
  (let ((system (make-foreign-procedure s_system 1)))
    (lambda (cmd) (system (%string->utf8 cmd #t)))))

(define (simplify-bytevector e)
  (cond
    ((bytevector? e)
     `(bytevector . ,(bytevector->list e)))
    ((not (pair? e)) e)
    ((not (eq? (car e) 'quote))
     (cons (simplify-bytevector (car e))
           (simplify-bytevector (cdr e))))
    (else e)))

(define get-process-id (make-foreign-procedure s_getpid 0))
(define-macro (import . _) 0)

(define-macro (cond-include pred e)
  (if (eval pred)
      e
      0))

(define-macro (let-values bs . es)
  (if (not (= 1 (length bs)))
      (error "let-values" "only-one-binding" bs)
      (let ((params (caar bs))
            (e (cadar bs)))
        `(call-with-values
          (lambda () ,e)
          (lambda ,params . ,es)))))

(define-macro (let*-values bs . es)
  (let recur ((bs bs))
    (if (pair? bs)
        `(let-values (,(car bs)) ,(recur (cdr bs)))
        (make-begin es))))

(cond-include (equal? (getenv "RIDER") "KICK")
  (match (cdr (command-line))
    (("-E" ,out . ,inp)
     (system (format "rm -f ~a" out))
     (define op (open-output-file out))
     (for-each
      (lambda (inp)
       (for-each (lambda (e) (writeln (simplify-bytevector (uniquify* e)) op)) (read-sexps-from-path inp)))
      inp)
     (close-port op))
    ((,inp . ,inps)
     (for-each load (cons inp inps)))
    (,ln (error (car (command-line)) "unmatch" ln))))