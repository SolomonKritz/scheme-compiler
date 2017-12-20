(apply
 (apply (lambda (q) "hello")
        ((lambda z  z)(apply (lambda (x y) y) '(1 2))))
 'symbol)