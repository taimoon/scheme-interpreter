(let ()
(define (make-coroutine-generator proc)
  ; proc is a procedure that takes one argument, yield.
  (define resume #f)
  (define return #f)
  (define yield
    (lambda (v)
      (call/cc (lambda (r) (set! resume r) (return v)))))
  (define (coro)
    (call/cc (lambda (cc)
      (set! return cc)
      (if (not resume)
          (begin
            (proc yield)
            (set! coro (lambda () (eof-object)))
            (return (eof-object)))
          (resume 0)))))
  (lambda () (coro)))

(define (make-iota-gen end)
  (make-coroutine-generator (lambda (yield)
    (let iter ((start 0))
      (if (< start end)
          (begin
            (yield start)
            (iter (+ start 1))))))))

(define (glast g)
  (let iter ((l #f) (r (g)))
    (if (eq? r (eof-object))
        l
        (iter r (g)))))

(write (glast (make-iota-gen 12345)))
(newline)
)