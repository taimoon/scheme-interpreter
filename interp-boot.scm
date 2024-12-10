(set! list (lambda x x))
(load "interp-preprocess.scm")
(load "interp-kernel.scm")
(load "lib/scheme-libs.scm")

(define (preprocess inp out)
  (let ((op (open-output-file out)))
    (map (lambda (e) (writeln e op))
      (cdr (uniquify-program (cons 'begin (read-sexps-from-path inp)))))
    (close-port op)))

(system "mkdir -p preprocess")
(system "mkdir -p preprocess/lib")

(if (file-exists? "preprocess/interp-preprocess.scm")
    #t
    (begin
      (preprocess "interp-preprocess.scm" "preprocess/interp-preprocess.scm")
      (preprocess "interp-kernel.scm" "preprocess/interp-kernel.scm")))

(define compiler-path "scheme-compiler")
(load "preprocess/interp-preprocess.scm")
(load "preprocess/interp-kernel.scm")

(define (for-each f xs)
  (if (pair? xs)
      (begin
        (f (car xs))
        (for-each f (cdr xs)))
      '()))

(define files '(
  "lib/scheme-libs.scm"
  "lib/reader.scm"
  "lib/writer.scm"
  "lib/match-defmacro.scm"
  "lib/set.scm"
  "lib/utils.scm"
))

(define (preprocess* name)
  (preprocess (string-append compiler-path "/" name)
              (string-append "preprocess/" name)))

(for-each
  (lambda (name)
    (preprocess* name)
    (load (string-append "preprocess/" name)))
  files)
(preprocess* "front.scm")
(preprocess* "compiler.scm")
