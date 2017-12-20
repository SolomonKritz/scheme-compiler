(let ([x (make-vector 6 '())])
  (if (null? (vector-ref x 5))
      (vector-ref x 10)
      42))