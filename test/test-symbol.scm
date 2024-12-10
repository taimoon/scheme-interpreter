(define (write-line x)
  (write x)
  (newline))
(write-line 'bad)
(write-line 'apple)
(write-line (symbol? 'bad))
(write-line (symbol? "bad"))
(write-line (eq? 'reimu 'reimu))
(write-line (eq? 'reimu "reimu"))
(write-line '(1 2 3))
(write-line 'x)
(write-line 'y)
(write-line
  '(xyz moe reimu marisa
    1 2 3 #t #f #\A #\a
    "something" "bad"))