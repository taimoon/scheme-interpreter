(writeln
 ((lambda x (+ (car x) (car (cdr x))))
 2 3))