(define %extended-alphabets
  '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~))

(define (memq? x s)
  (if (memq x s) #t #f))

(define str-buf (make-string 512))

(define (%next-token p)
  ; return datum as datum; otherwise list-structured token such as
  ; (left-paren)
  ; (right-paren)
  ; (quote) (unquote) (unquote-splicing) (quasiquote)
  (let ([c (read-char p)])
    (cond 
      [(eof-object? c) c]
      [(eq? c #\') '(quote)]
      [(eq? c #\`) '(quasiquote)]
      [(eq? c #\,)
       (if (eq? (peek-char p) #\@)
           (begin (read-char p) '(unquote-splicing))
           '(unquote))]
      [(eq? c #\.)
       (let ([c (peek-char p)])
        (if (or (char-alphabetic? c) (memq? c %extended-alphabets))
            (begin (unread-char c p) (read-identifier p))
            '(dot)))]
      [(eq? c #\") (read-quote-delimited-string p)]
      [(eq? c #\;) (read-comment p)]
      [(eq? c #\#) (read-hash-prefixed p)]
      [(eq? c #\-)
       (if (not (char-numeric? (peek-char p)))
           (begin (unread-char c p) (read-identifier p))
           (- (read-number p 10)))]
      [(char-numeric? c) (unread-char c p) (read-number p 10)]
      [(or (char-alphabetic? c) (memq? c %extended-alphabets))
       ;;; see r6rs lexical syntax
       (unread-char c p)
       (read-identifier p)]
      [(left-paren? c) '(left-paren)]
      [(right-paren? c) '(right-paren)]
      [(char-whitespace? c) (%next-token p)]
      [else (error '%next-token 'unknown:lexeme c)])))

(define read-quote-delimited-string (let ()
  (define (read-quote-delimited-string p)
    (define %escaped-string-element
      (list
        (list #\" #\")
        (list #\a (integer->char 7))  ; #\alarm
        (list #\b (integer->char 8))  ; #\backspace
        (list #\t (integer->char 9))  ; #\tab
        (list #\n (integer->char 10)) ; #\linefeed
        (list #\v (integer->char 11)) ; #\vtab
        (list #\f (integer->char 12)) ; #\page
        (list #\r (integer->char 13)) ; #\return
        (list #\\ #\\)
      ))
    (let loop ([i 0] [c (read-char p)] [acm '()])
      (cond 
        [(eof-object? c) (error 'read-quote-delimited-string 'open-string c)]
        [(eq? c #\")
        (apply string-append (reverse (cons (substring str-buf 0 i) acm)))]
        [(eq? c #\\)
        (let* ((c (read-char p))
                (esc-c (assq c %escaped-string-element)))
            (if esc-c
                (string-set! str-buf i (cadr esc-c))
                (error "unknown escape character" c))
            (loop (add1 i) (read-char p) acm))]
        [(>= i (string-length str-buf))
        (loop 0 c (cons (substring str-buf 0 i) acm))]
        [else
        (string-set! str-buf i c)
        (loop (add1 i) (read-char p) acm)])))
  read-quote-delimited-string))

(define (read-comment p)
  (let loop ([c (read-char p)])
    ; #\newline 10
    (if (or (eof-object? c) (eq? c (integer->char 10)))
        (%next-token p)
        (loop (read-char p)))))

(define (read-block-comment p)
  (let loop ([c (read-char p)])
    (cond 
      [(eof-object? c)
       (error 'read-block-comment "expect closing block comment")]
      [(and (eq? c #\|) (eq? (peek-char p) #\#))
       (read-char p) (%next-token p)]
      [(and (eq? c #\#) (eq? (peek-char p) #\|))
       (read-char p)
       (read-block-comment p)
       (read-block-comment p)]
      [else (loop (read-char p))])))

(define (read-hash-prefixed p)
  (let ([c (read-char p)])
    (cond 
      [(eof-object? c) c]
      [(eq? (char-downcase c) #\t) #t]
      [(eq? (char-downcase c) #\f) #f]
      [(eq? (char-downcase c) #\b) (read-number p 2)]
      [(eq? (char-downcase c) #\o) (read-number p 8)]
      [(eq? (char-downcase c) #\d) (read-number p 10)]
      [(eq? (char-downcase c) #\x) (read-number p 16)]
      [(eq? c #\\) (read-literal-char p)]
      [(eq? c #\() (list 'vector)]
      [(eq? c #\|) (read-block-comment p)]
      [(eq? c #\;) (parse (tokenizer p)) (%next-token p)]
      [else (error 'read-hash-prefixed 'unknown:lexeme c)])))

(define %name-characters
  (map (lambda (b) (append (string->list (car b)) (list (integer->char (cadr b)))))
    '(
      ("alarm" 7)
      ("backspace" 8)
      ("tab" 9)
      ("space" 32)
      ("newline" 10)
      ("linefeed" 10)
      ("vtab" 11)
      ("page" 12)
      ("return" 13)
      ("nul" 0)
      ("esc" 27)
      ("delete" 127)
    )))

(define (naive-lookahead-match x xss peek next)
  ;;; return first match
  (cond
    [(not (pair? xss)) #f]
    [(not (pair? (car xss)))
     (error "naive-lookahead-match" "ill-formed" (car xss))]
    [(and (eq? x (car (car xss)))
          (pair? (cdr (car xss)))
          (not (pair? (cdr (cdr (car xss))))))
     (cadr (car xss))]
    [else
     (let* ([xss (filter (lambda (s) (eq? (car s) x)) xss)])
      (next)
      (naive-lookahead-match (peek) (map cdr xss) peek next))]))

(define (read-literal-char p)
  (let* ([c (read-char p)]
         [next-chars
          (lambda (c css)
            (filter (lambda (cs) (eq? (car cs) c)) css))]
         [css (next-chars c %name-characters)]
         [css* (map cdr css)])
    (cond
      [(and (pair? css)
            (pair? (next-chars (peek-char p) css*)))
        (naive-lookahead-match (read-char p) css* (lambda () (read-char p)) (lambda () (peek-char p)))]
      [else c])))

(define (read-number p radix)
  (let loop ([i 0]
             [c (read-char p)])
  (cond [(eof-object? c)
         (string->number (substring str-buf 0 i) radix)]
        [(or (and (= radix 16)
                  (or (and (char<=? #\0 c) (char<=? c #\9))
                      (and (char<=? #\a (char-downcase c)) (char<=? (char-downcase c) #\f))))
             (and (= radix 10) (char<=? #\0 c)  (char<=? c #\9))
             (and (= radix 8)  (char<=? #\0 c) (char<=? c #\7))
             (and (= radix 2)  (char<=? #\0 c) (char<=? c #\1)))
         (string-set! str-buf i c)
         (loop (add1 i) (read-char p))]
        [else (unread-char c p) (string->number (substring str-buf 0 i) radix)])))

(define (read-identifier p)
  (let loop ([i 0]
             [c (read-char p)])
  (cond [(eof-object? c)
         (string->symbol (substring str-buf 0 i))]
        [(or (char-alphabetic? c)
             (char-numeric? c)
             (memq? c %extended-alphabets))
         (string-set! str-buf i c)
         (loop (add1 i) (read-char p))]
        [else (unread-char c p) (string->symbol (substring str-buf 0 i))])))

(define left-paren
  '(#\( #\[))

(define right-paren
  '(#\) #\]))

(define (left-paren? x)
  (memq? x left-paren))

(define (right-paren? x)
  (memq? x right-paren))

(define (tokenizer p)
  (let ([buf '()])
    (lambda (msg)
      (cond
        [(eq? msg 'peek)
         (if (pair? buf)
             (car buf)
             (let ([res (%next-token p)])
              (set! buf (list res))
              res))]
        [(eq? msg 'next)
         (if (pair? buf)
             (let ([res (car buf)])
              (set! buf '())
              res)
             (%next-token p))]
        [else (error 'tokenizer "unknown msg" msg)]))))

(define (tokenizer-next tk) (tk 'next))

(define (tokenizer-peek tk) (tk 'peek))

(define (parse-list tk)
  (let ([v (tokenizer-peek tk)])
    (cond
      [(equal? v '(right-paren))
       (tokenizer-next tk) '()]
      [(equal? v '(dot))
       (tokenizer-next tk)
       (let* ([res (parse tk)]
              [c (tokenizer-peek tk)])
        (if (not (equal?  c '(right-paren)))
            (error "parse-list" "expect closing right-paren" c)
            (tokenizer-next tk))
        res)]
      [else
       (let* ([left (parse tk)]
              [right (parse-list tk)])
        (cons left right))])))

(define (lexing-all p)
  (let loop ([c (%next-token p)] [acm '()])
    (if (eof-object? c)
        (reverse acm)
        (loop (%next-token p) (cons c acm)))))

(define (parse tk)
  (let ([v (tokenizer-next tk)])
    (cond
      [(and (pair? v) (null? (cdr v))
            (memq (car v) '(quote unquote quasiquote unquote-splicing)))
       (list (car v) (parse tk))]
      [(or (string? v) (char? v) (boolean? v) (integer? v) (symbol? v) (eof-object? v))
       v]
      [(equal? v '(left-paren))
       (parse-list tk)]
      [(equal? v '(vector))
       (list->vector (parse-list tk))]
      [else (error "parse" "parse-error" v)])))

(define read
  (case-lambda
    (() (read (current-input-port)))
    ((port) (parse (tokenizer port)))))
