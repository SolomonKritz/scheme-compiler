(define substring1 (substring "hello" 1))
(define substring2 (substring "hello" 1 3))
(if (and (eq? substring2 "ell") (eq? substring1 "ell0"))
    (apply substring '("Now we are engaged in a great civil war, testing whether that nation, 
or any nation so conceived and so dedicated, can long endure.
We are met on a great battle-field of that war.
We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live.
It is altogether fitting and proper that we should do this." 50 60))
      (apply substring  '("Compilers 4 Life" 3)))