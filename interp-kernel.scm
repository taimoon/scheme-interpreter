(define (open-input-file file)
  (fopen file "r"))

(define (open-output-file file)
  (fopen file "w"))

(define (close-port p)
  (fclose p))

(define (close-output-port p)
  (close-port p))

(define (close-input-port p)
  (close-port p))

(define (read-sexps-from-input inp)
  (let ((obj (read inp)))
    (if (eof-object? obj)
        '()
        (cons obj (read-sexps-from-input inp)))))

(define (read-sexps-from-path path)
  (let* ((inp (open-input-file path))
         (res (read-sexps-from-input inp)))
    (close-port inp)
    res))

(define (peek-char p)
  (let ((c (read-char p)))
    (unread-char c p)
    c))

(define (error . args)
  (writeln "error: ")
  (writeln args)
  (exit 1))
