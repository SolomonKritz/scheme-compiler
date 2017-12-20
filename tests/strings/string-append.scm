(define abc (apply string-append '("a" "b" "c")))
(define hello-list2 '(#\h #\e #\l #\l #\o))
(if (eq? abc (string-append "abc" "WRONG"))
    (eq? (string-append "c" "o" "r" "r" "ect") "correct")
    (string-append "Four score and seven years ago our fathers brought forth"
                   "on this continent, a new nation, conceived in Liberty,"
                   "and dedicated to the proposition that all men are created equal."))