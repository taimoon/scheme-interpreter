(define fread
  (let ((fread (make-foreign-procedure s_fread 4)))
    (lambda (buf start end %ip)
      (fread buf start end %ip))))

(define fwrite
  (let ((fwrite (make-foreign-procedure s_fwrite 4)))
    (lambda (buf start end %op)
      (fwrite buf start end %op))))

(define %read-char (let ()
(define (%read-char-u8 ip)
  (let ((b (read-u8 ip)))
    (cond
      ((eof-object? b) (error "%read-char" "expect-more-byte"))
      ((not (= (ash b -6) #b10)) (error "%read-char" "bad-byte" (ash b -6) b))
      (else b))))
(define (%read-char ip)
    (let ((b0 (read-u8 ip)))
      (cond
        ((eof-object? b0) b0)
        ((= 0 (ash b0 -7)) (integer->char b0))
        ((= #b110 (ash b0 -5))
         (let* ((b1 (%read-char-u8 ip))
                (xxx (bitwise-and #b111 (ash b0 -2)))
                (yyyy
                 (bitwise-ior
                   (ash (bitwise-and #b11 b0) 2)
                   (bitwise-and #b11 (ash b1 -4))))
                (zzzz (bitwise-and #b1111 b1)))
          (integer->char
               (bitwise-ior
                 (ash xxx 8)
                 (ash yyyy 4)
                 zzzz))))
        ((= #b1110 (ash b0 -4))
         (let* ((b1 (%read-char-u8 ip))
                (b2 (%read-char-u8 ip))
                (zzzz (bitwise-and #b1111 b2))
                (yyyy
                  (bitwise-ior
                    (ash (bitwise-and #b11 b1) 2)
                    (bitwise-and #b11 (ash b2 -4))))
                (xxxx (bitwise-and #b1111 (ash b1 -2)))
                (wwww (bitwise-and #b1111 b0)))
            (integer->char
              (bitwise-ior
                (ash wwww 12)
                (ash xxxx 8)
                (ash yyyy 4)
                zzzz))))
        ((= #b11110 (ash b0 -3))
         (let* ((b1 (%read-char-u8 ip))
                (b2 (%read-char-u8 ip))
                (b3 (%read-char-u8 ip))
                (zzzz (bitwise-and #b1111 b3))
                (xxxx (bitwise-and #b1111 (ash b2 -2)))
                (yyyy
                 (bitwise-ior
                    (ash (bitwise-and #b11 b2) 2)
                    (bitwise-and #b11 (ash b3 -4))))
                (wwww (bitwise-and #b1111 b1))
                (vvvv
                  (bitwise-ior
                    (ash (bitwise-and #b11 b0) 2)
                    (bitwise-and #b11 (ash b1 -4))))
                (u (bitwise-and #b1 (ash b0 -2))))
            (integer->char
              (bitwise-ior
                (ash u 20)
                (ash vvvv 16)
                (ash wwww 12)
                (ash xxxx 8)
                (ash yyyy 4)
                zzzz))
            ))
        (else (error %read-char "unknown-utf-8")))))
%read-char))

(define (make-port p handler buf)
  (vector make-port p 0 handler buf))

(define (port? p)
  (and (vector? p)
       (= (vector-length p) 5)
       (eq? (vector-ref p 0) make-port)))

(define (port-handler p)
  (if (port? p)
      (vector-ref p 3)
      (error "port-handler" "expect-port" p)))

(define (set-port-handler! p handler)
  (if (port? p)
      (vector-set! p 3 handler)
      (error "set-port-handler!" "expect-port" p)))

(define (port-input-buffer p)
  (if (port? p)
      (vector-ref p 4)
      (error "port-input-buffer" "expect-port" p)))

(define (port-output-buffer p)
  (if (port? p)
      (vector-ref p 4)
      (error "port-output-buffer" "expect-port" p)))

(define (port-input-index p)
  (if (port? p)
      (vector-ref p 2)
      (error "port-input-index" "expect-port" p)))

(define (set-port-input-index! p i)
  (if (port? p)
      (vector-set! p 2 i)
      (error "set-port-input-index!" "expect-port" p)))

(define (%port p) (vector-ref p 1))

(define %binary-input-port-handler
  (let ((char-buf '()))
    (define (%binary-input-port-handler msg ip . args)
      (match (cons msg args)
        ((read-u8)
         (let* ((buf (port-input-buffer ip))
                (sz (fread buf 0 1 (%port ip))))
           (cond
             ((eof-object? sz) (eof-object))
             ((integer? sz) (bytevector-u8-ref buf 0))
             (else (error "%binary-input-port-handler" "error")))))
        ((unread-char ,ch)
         (set! char-buf (cons ch char-buf)))
        ((peek-char)
         (if (pair? char-buf)
             (car char-buf)
             (let ((ch (read-char ip)))
              (set! char-buf (cons ch char-buf))
              ch)))
        ((read-char)
         (if (pair? char-buf)
             (let ((ch (car char-buf)))
              (set! char-buf (cdr char-buf))
              ch)
             (%read-char ip)))
        ((,msg . ,())
         (error "%binary-input-port-handler" "unknown-message" msg))))
  %binary-input-port-handler))

(define (%binary-output-port-handler msg op . args)
  (match (cons msg args)
    ((write-char ,ch)
     (let ((buf (port-output-buffer op)))
      (fwrite buf 0 (%utf32->utf8! ch buf 0) (%port op))))
    ((,msg . ,())
     (error "%binary-output-port-handler" "unknown-message" msg))))

(define current-input-port
  (let ((ip (make-port stdin %binary-input-port-handler (make-bytevector 1))))
    (lambda () ip)))

(define current-error-port
  (let ((op (make-port stderr %binary-output-port-handler (make-bytevector 8))))
    (lambda () op)))

(define current-output-port
  (let ((op (make-port stdout %binary-output-port-handler (make-bytevector 8))))
    (lambda () op)))

(define fopen
  (let ((fopen (make-foreign-procedure s_fopen 2)))
    (lambda (path mode) (fopen (%string->utf8 path #t) (%string->utf8 mode #t)))))

(define fclose (make-foreign-procedure s_fclose 1))

(define (file-exists? path)
  (let ((fp (fopen path "r")))
    (if (eq? fp 0)
        #f
        (begin (fclose fp) #t))))

(define open-input-file
  (lambda (path)
    (let ((fp (fopen path "r")))
      (if (eq? fp 0)
          (error "open-input-file" "file does not exists" path)
          (make-port fp %binary-input-port-handler (make-bytevector 1))))))

(define open-output-file
  (lambda (path)
    (if (file-exists? path)
        (error "open-output-file" "file exists" path)
        (make-port (fopen path "w") %binary-output-port-handler (make-bytevector 8)))))

(define open-binary-input-file
  (lambda (path)
    (if (file-exists? path)
        (make-port (fopen path "rb") %binary-input-port-handler (make-bytevector 1))
        (error "open-binary-input-file" "file does not exists" path))))

(define open-binary-output-file
  (lambda (path)
    (if (file-exists? path)
        (error "open-binary-output-file" "file exists" path)
        (make-port (fopen path "wb") %binary-output-port-handler (make-bytevector 8)))))

(define (close-port fp) (fclose (%port fp)))

(define (read-u8 ip)
  (if (not (port? ip))
      (error "read-u8" "expect port" ip)
      ((port-handler ip) 'read-u8 ip)))

(define (%bytevector-input-handler msg ip . args)
  (match (cons msg args)
    ((read-u8)
     (let ((buf (%port ip))
           (i (port-input-index ip)))
      (if (>= i (bytevector-length buf))
          (eof-object)
          (let ((r (bytevector-u8-ref buf i)))
            (set-port-input-index! ip (add1 i))
            r))))
    ((read-char)
     (%read-char ip))
    (,() (error "%bytevector-input-handler" "unknown message" msg))))

(define (open-bytevector-input-port bytevector)
  (make-port bytevector %bytevector-input-handler #f))

(define read-char
  (case-lambda
    (() (read-char (current-input-port)))
    ((ip) ((port-handler ip) 'read-char ip))))

(define (unread-char ch ip)
  ((port-handler ip) 'unread-char ip ch))

(define (peek-char ip)
  ((port-handler ip) 'peek-char ip))

(define write-char
  (case-lambda
    ((ch) (write-char ch (current-output-port)))
    ((ch op) ((port-handler op) 'write-char op ch))))

(define newline
  (case-lambda
    (() (write-char (integer->char 10) (current-output-port)))
    ((op) (write-char (integer->char 10) op))))

(define (utf8->string b)
  (define ip (open-bytevector-input-port b))
  (let recur ((i 0)
              (ch (read-char ip)))
    (if (eof-object? ch)
        (make-string i)
        (let ((s (recur (add1 i) (read-char ip))))
          (string-set! s i ch)
          s))))

(define flush-output-port
  (let ((flush (make-foreign-procedure s_fflush 1)))
    (case-lambda
      (() (flush-output-port (current-output-port)))
      ((op) (flush (%port op))))))
