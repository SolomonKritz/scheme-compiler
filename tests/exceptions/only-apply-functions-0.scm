(let ([ident (lambda (x) x)])
  (set! ident (ident 6))
  (ident "oops"))