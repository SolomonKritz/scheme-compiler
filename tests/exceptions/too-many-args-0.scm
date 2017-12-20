(define (proc x y z) (x y z))
(define take-2 (proc (lambda (p q) p) (lambda (a b c) 2) "hello"))
(take-2 "one" "two" "three")