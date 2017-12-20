(let* ([str "hello"][chr (string-ref str 4)])
  (set! chr #\q)
  (if (apply string-ref '("toast" 1)) str "err"))