(let loop ([i 0] [x (make-vector 14 '())])
  (vector-ref x i)
  (loop (+ 1 i) x))