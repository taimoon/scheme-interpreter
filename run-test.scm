(define (system* cmd . args)
  (let ((res (system (apply format (cons cmd args)))))
    (if (= res 0)
        res
        (exit res))))
(define (tests)
  (list
    "test-lit"
    "test-unary"
    "test-binary-cmp"
    "test-arithmetic"
    test-command-line
    "test-let"
    "test-if"
    "test-logic-conn"
    "test-cond"
    "test-pair"
    "test-string"
    "test-vector"
    "test-simple-procedure"
    "test-simple-rec-proc"
    "test-mutual-procedure"
    "test-procedure-ref"
    "test-lambda"
    "test-lambda-1"
    "test-tail-sum"
    ; "test-tail-add"  
    "test-simple-set"
    "test-free-var-set"
    "test-letrec"
    "test-symbol"
    "test-named-let"
    test-ffi-io
    "test-vararg"
    "test-apply"
    "test-tail-apply"
    "test-operand-prim"
    "test-bitwise"
    "test-quasiquote"

    test-inp
    "test-variadic"
    "test-recur"
    "test-macro"
    
    "test-case-lambda-0"
    "test-case-lambda-1"
    "test-case-lambda-2"
    "test-match"

    "test-simple-gc-0"
    "test-simple-gc-1"
    "test-simple-gc-2"
    "test-simple-gc-3"
  ))
(system* "gcc -Wall main.c interp.c -o main.out")

(define (test-command-line)
  (define f "./test/test-command-line")
  (system* "diff ~a.txt <(./interp.out interp.scm ~a.scm reimu marisa -19 2 3 5)" f f))

(define (test-ffi-io)
  (display "test/test-ffi-io.scm\n")
  (system* "diff test/test-ffi-io.scm <(./interp.out interp.scm test/test-ffi-io.scm)")
  (system* "diff test/test-ffi-io.scm test/test-ffi-io.txt"))

(define (test-inp)
  (display "test/inp-main.scm\n")
  (system* "diff <(./interp.out interp.scm test/inp-main.scm) test/inp-main.txt")
  (let ()
    (define res
      (equal?
        (read (open-input-file "test/out.scm"))
        (read (open-input-file "test/inp.scm"))))
    (if res
        (system "rm -f test/out.scm")
        (exit 1))))

(for-each
  (lambda (f)
    (if (procedure? f)
        (f)
        (begin
          (display f) (newline)
          (system* "diff <(./interp.out interp.scm test/~a.scm) test/~a.txt" f f))))
  (tests))
