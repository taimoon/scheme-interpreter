(define list (lambda x x))
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (p) (car (cdr p))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define null? (lambda (x) (eq? x '())))
(define not (lambda (e) (eq? e #f)))
(define length (lambda (xs) (if (pair? xs) (+ 1 (length (cdr xs))) 0)))
(define integer? (lambda (e) (if (fixnum? e) #t (bignum? e))))
(define eof-object ((lambda (eof) (lambda () eof)) eof))
(define eof-object? ((lambda (eof) (lambda (v) (eq? eof v))) eof))
(define void (lambda () (if #f #f)))

(define (assq x xs)
  (if (null? xs)
      #f
      (if (eq? x (caar xs))
          (car xs)
          (assq x (cdr xs)))))

(define expanders '())

(define (expanders-add-core! kw p)
  (set! expanders
        (cons (cons kw p) expanders)))

(define (expanders-add-macro! kw p)
  (set! expanders
        (cons (cons kw (lambda (env . es) (expand (apply p es) env))) expanders)))

(define (map* f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map* f (cdr xs)))
      (if (null? xs)
          '()
          (f xs))))

(define (expand e env)
  (if (pair? e)
      (if (symbol? (car e))
          ((lambda (r)
           (if (if (pair? r) (procedure? (cdr r)) #f)
               (apply (cdr r) (cons env (cdr e)))
               (map* (lambda (e) (expand e env)) e)))
           (assq (car e) env))
          (map* (lambda (e) (expand e env)) e))
      e))

(define (make-begin es)
    (if (pair? (cdr es))
        (cons 'begin es)
        (car es)))

(expanders-add-macro! 'define-macro
  (lambda (var val . es)
    (if (symbol? var)
        (list 'expanders-add-macro! (list 'quote var) val)
        (list 'expanders-add-macro!
          (list 'quote (car var))
          (list 'lambda (cdr var) (make-begin (cons val es)))))))

(define eval
  ((lambda (eval)
    (lambda (e) (eval (expand e expanders))))
   eval))

(define (map f xs)
  (if (pair? xs)
      (cons (f (car xs)) (map f (cdr xs)))
      '()))

(define (let->lambda bs e)
  (cons
    (list 'lambda (map car bs) e)
    (map cadr bs)))

(define (named-let->letrec fn bs es)
  (list 'letrec (list (list fn (list 'lambda (map car bs) (make-begin es))))
    (cons fn (map cadr bs))))

(define-macro (let bs . es)
  (if (symbol? bs)
      (named-let->letrec bs (car es) (cdr es))
      (let->lambda bs (make-begin es))))

(define (append xs ys)
  (if (pair? xs)
      (cons (car xs) (append (cdr xs) ys))
      ys))

(define-macro (letrec bs . es)
  (cons
    (list 'lambda (map car bs)
      (make-begin
        (append
          (map (lambda (e) (cons 'set! e)) bs)
          es)))
    (map (lambda _ 0) bs)))

(define-macro (or . es)
  (let or->if ((es es))
    (if (pair? es)
      (list 'if (car es) #t (or->if (cdr es)))
      #f)))

(define-macro (and . es)
  (let and->if ((es es))
    (if (pair? es)
        (list 'if (car es) (and->if (cdr es)) #f)
        #t)))

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

(define-macro cond (lambda (clause . clauses) (cond-clauses->ifs (cons clause clauses))))

(define (extend-env xs vs env)
  (cond
    ((null? xs) env)
    ((symbol? xs) (cons (list xs vs) env))
    ((not (pair? xs)) (error "extend-env" "ill-form" xs vs))
    (else
     (cons (list (car xs) (car vs)) (extend-env (cdr xs) (cdr vs) env)))))

(define (maybe-apply-env x xs) (assq x xs))

(expanders-add-core! 'quote
  (lambda (env e) (list 'quote e)))
(expanders-add-core! 'begin
  (lambda (env . es) (make-begin (expand-each es env))))
(expanders-add-core! 'lambda
  (lambda (env params . es)
    (list 'lambda params (expand (make-begin es) (extend-env params params env)))))
(expanders-add-core! 'set!
  (lambda (env var val) (list 'set! (expand var env) (expand val env))))
(expanders-add-core! 'define
  (lambda (env var . val)
    (if (symbol? var)
        (list 'define var (expand (car val) (extend-env var var env)))
        (expand (list 'define (car var) (list 'lambda (cdr var) (make-begin val))) env))))

(define (expand-each es env)
  (map (lambda (e) (expand e env)) es))

(define (expand e env)
  (cond
    ((not (pair? e)) e)
    ((symbol? (car e))
     (let ((r (assq (car e) env)))
      (if (and (pair? r) (procedure? (cdr r)))
          (apply (cdr r) (cons env (cdr e)))
          (expand-each e env))))
    (else (expand-each e env))))

(define (let*->let-aux bindings body)
  (if (null? (cdr bindings))
      (list 'let (list (car bindings)) body)
      (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))

(define-macro let* (lambda (bs . es) (let*->let-aux bs (make-begin es))))
  
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

(define-macro quasiquote expand-qq)

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

(define-macro case-lambda
  (lambda (clause . clauses)
    (if (pair? clauses)
        (case-lambda->lambdas (cons clause clauses))
        (cons 'lambda clause))))

(define expand* (lambda (e) (expand e expanders)))

(define apply
  (let ((apply apply))
    (define (cons* x xs)
      (if (pair? xs)
          (cons x (cons* (car xs) (cdr xs)))
          x))
    (lambda (fn v . vs)
      (apply fn (cons* v vs)))))

(define values (lambda vs (call/cc (lambda (k) (apply k vs)))))

(define (make-guardian)
  (let ((tc (let ((x (cons #f '()))) (cons x x))))
    (lambda args
      (if (null? args)
          (if (eq? (car tc) (cdr tc))
              #f
              (let ((x (car tc)))
                (let ((y (car x)))
                  (set-car! tc (cdr x))
                  (set-car! x #f)
                  (set-cdr! x #f)
                  y)))
          (install-guardian (car args) tc)))))

(define POST-GC-HANDLERS '())
(define (post-gc-handler-register f)
  (set! POST-GC-HANDLERS (cons f POST-GC-HANDLERS)))
(define (post-gc-handler-trigger)
  (let loop ((handlers POST-GC-HANDLERS))
    (if (pair? handlers)
        (begin ((car handlers)) (loop (cdr handlers))))))
(define collect
  (let ((collect collect))
    (lambda ()
      (let ((v (collect)))
        (post-gc-handler-trigger)
        v))))

(define (%ascii->utf8 s null-term?)
  (let loop ((i 0)
             (buf (make-bytevector (+ (string-length s) (if null-term? 1 0)) 0)))
    (if (>= i (string-length s))
        buf
        (begin
          (bytevector-u8-set! buf i (char->integer (string-ref s i)))
          (loop (+ i 1) buf)))))

(define-macro (cond-eval pred . es)
  (if (eval pred)
      (cons 'begin es)
      0))

(cond-eval (not unicode-support?)
  (define %string->utf8 %ascii->utf8)
  (define (%utf32->utf8! c32 buf off)
    (let ((c32 (char->integer c32)))
      (cond
        ((<= c32 #x7F)
          (bytevector-u8-set! buf (+ off 0) c32)
          1)
        (else 0))))
)

(cond-eval unicode-support?
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
)

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

(define (list->vector xs)
  (let recur ((i 0) (xs xs))
    (if (pair? xs)
        (let ((v (recur (+ i 1) (cdr xs))))
          (vector-set! v i (car xs))
          v)
        (make-vector i))))

(define (vector . xs) (list->vector xs))

(cond-eval hosted?
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
    ((lambda x x) '%make-foreign-procedure (%string->utf8 (symbol->string fn) #t) argc)))

(cond-eval hosted?
  (define abort (make-foreign-procedure abort 0)))

(cond-eval free-standing?
  (define abort (lambda () (exit -1))))

(define error
  (let ((write write)
        (newline newline))
    (lambda x
      (write "error: " stderr)
      (write x stderr)
      (newline stderr)
      (abort))))

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

(define (vector=? v w)
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
         (vector=? x y))
        ((and (string? x) (string? y))
         (string=? x y))
        (else #f)))

(cond-eval hosted?
  (define fwrite (make-foreign-procedure s_sys_fwrite 4))

  (define fread (make-foreign-procedure s_sys_fread 4))

  (define fopen
    (let ((fopen (make-foreign-procedure s_sys_fopen 2)))
      (lambda (path mode)
        (fopen (%string->utf8 path #t) (%string->utf8 mode #t)))))

  (define fclose (make-foreign-procedure s_sys_fclose 1))

  (define getenv
    (let ((getenv (make-foreign-procedure s_sys_getenv 1)))
      (lambda (var)
        (let ((r (getenv (%string->utf8 var #t))))
          (if r
              (utf8->string r)
              r)))))

  (define setenv
    (let ((setenv (make-foreign-procedure s_sys_setenv 2)))
      (lambda (var val)
        (setenv (%string->utf8 var #t) (%string->utf8 val #t)))))
  
  (define parse-file
    (let ((parse-file (make-foreign-procedure _parse_file 1)))
      (lambda (path)
        (parse-file (%string->utf8 path #t)))))

  (define (read-sexps-from-path path) (parse-file path))
  (define (for-each f xs)
    (let loop ((xs xs))
      (if (pair? xs)
          (begin (f (car xs)) (loop (cdr xs))))))
  (define load
    (lambda (path)
      (for-each (lambda (e) (eval (expand* e))) (read-sexps-from-path path))))
  
  (define-macro (include path)
    (make-begin (read-sexps-from-path path)))
) ;; cond-eval hosted?

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

(include "lib/match-defmacro.scm")
(define-macro (match . e) (compile-match (cons 'match e)))

(cond-eval hosted?
(define (string->list s)
  (let recur ((i 0))
    (if (< i (string-length s))
        (cons (string-ref s i) (recur (+ i 1)))
        '())))
(include "lib/unicode.scm")
(define (abs x) (if (< x 0) (- x) x))
(define (min x y) (if (< x y) x y))
(define (max x y) (if (< x y) y x))
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
  (let ((system (make-foreign-procedure s_sys_system 1)))
    (lambda (cmd) (system (%string->utf8 cmd #t)))))

(define get-process-id (make-foreign-procedure s_sys_getpid 0))
) ;; cond-eval hosted?

(define-macro (import . _) 0)

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

(cond-eval (equal? (getenv "RIDER") "KICK")
  (define (simplify-bytevector e)
    (cond
      ((bytevector? e)
      `(bytevector . ,(bytevector->list e)))
      ((not (pair? e)) e)
      ((not (eq? (car e) 'quote))
      (cons (simplify-bytevector (car e))
            (simplify-bytevector (cdr e))))
      (else e)))
  (match (cdr (command-line))
    ((,opt ,out . ,inp)
     (guard (or (equal? opt "-E") (equal? opt "-F")))
     (set! free-standing? (equal? opt "-F"))
     (set! hosted? (not free-standing?))
     (set! boot? #f)
     (system (format "rm -f ~a" out))
     (define op (open-output-file out))
     (for-each
      (lambda (inp)
       (for-each (lambda (e) (let ((e (simplify-bytevector (expand* e)))) (if (not (integer? e)) (writeln e op)))) (cons '(set! boot? #f) (read-sexps-from-path inp))))
      inp)
     (close-port op))
    ((,inp . ,inps)
     (for-each load (cons inp inps)))
    (,ln (error (car (command-line)) "unmatch" ln))))
