(let ((inp (open-input-file "./test/test-ffi-io.scm")))
  (let loop ((ch (read-char inp)))
    (if (eof-object? ch)
        (close-port inp)
        (begin
          (write-char ch)
          (loop (read-char inp))))))

(let ((inp (open-input-file "./test/test-ffi-io.scm"))
      (out (open-output-file "./test/test-ffi-io.txt")))
  (let loop ((ch (read-char inp)))
    (if (eof-object? ch)
        (begin
          (close-port inp)
          (close-port out))
        (begin
          (write-char ch out)
          (loop (read-char inp))))))
