(let ([takes-3 (lambda (x y z) 'sym)])
  (apply takes-3 '("a" "b" "c"))
  (apply takes-3 '("a" "b")))