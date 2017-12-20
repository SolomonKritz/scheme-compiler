#lang racket

; By Solomon Kritz

(provide top-level)
(require "utils.rkt")

(define (top-level e)
  (define (w-pul e)
    (match e
      [`(begin ,es ..1)
       (let ([defs (pul es)])
         (if (null? defs)
             `(begin ,@(map wo-pul es))
             `(letrec* ,(pul es) (begin ,@(map wo-pul es)))))]
      [else
       (trans e)]))

  (define (wo-pul e)
    (match e
      [`(begin ,es ..1)
       `(begin ,@(map wo-pul es))]
      [else
       (trans e)]))

  (define (trans e)
    
    (define (t-cond-cl clause)
      (match clause
        [`(else ,es ..1)
         `(else ,(w-pul `(begin ,@es)))]
        [`(,e0 ,es ..1)
         `(,(w-pul e0) ,(w-pul `(begin ,@es)))]
        [`(,e0)
         `(,(w-pul e0))]))
    
    (match e
      [`(letrec* ([,xs ,es] ...) ,bes ..1)
       `(letrec* (,@(map (lambda (x e0) `[,x ,(w-pul e0)]) xs es)) ,(w-pul `(begin ,@bes)))]
      [`(letrec ([,xs ,es] ...) ,bes ..1)
       `(letrec (,@(map (lambda (x e0) `[,x ,(w-pul e0)]) xs es)) ,(w-pul `(begin ,@bes)))]
      [`(let* ([,xs ,es] ...) ,bes ..1)
       `(let* (,@(map (lambda (x e0) `[,x ,(w-pul e0)]) xs es)) ,(w-pul `(begin ,@bes)))]
      [`(let ([,xs ,es] ...) ,bes ..1)
       `(let (,@(map (lambda (x e0) `[,x ,(w-pul e0)]) xs es)) ,(w-pul `(begin ,@bes)))]
      [`(let ,n ([,xs ,es] ...) ,bes ..1)
       `(let ,n (,@(map (lambda (x e0) `[,x ,(w-pul e0)]) xs es)) ,(w-pul `(begin ,@bes)))]
      [`(lambda (,(? symbol? xs) ... [,dxs ,des] ...) ,bes ..1)
       (define rest (gensym 'rest))
       (define dxs-des (map (lambda (x e0) `[,x ,(w-pul e0)]) dxs des))
       (if (null? dxs-des) `(lambda (,@xs) ,(w-pul `(begin ,@bes)))
           `(lambda (,@xs . ,rest)
              (letrec* (,@dxs-des)
                       ,(w-pul `(begin ,@(map (lambda (x) `(unless (null? ,rest) (set! ,x (car ,rest)) (set! ,rest (cdr ,rest)))) dxs)
                                       ,@bes)))))]
      [(and `(lambda ,(list-rest params ... rest) ,bes ..1) `(lambda ,(? cons? all-params) ,bes0 ..1))
       `(lambda (,@params . ,rest) ,(w-pul `(begin ,@bes)))]
      [`(lambda ,x ,bes ..1)
       `(lambda ,x ,(w-pul `(begin ,@bes)))]
      [`(dynamic-wind ,e0 ,e1 ,e2)
       `(dynamic-wind ,(w-pul e0) ,(w-pul e1) ,(w-pul e2))]
      [`(raise ,e0)
       `(raise ,(w-pul e0))]
      [`(delay ,e0)
       `(delay ,(w-pul e0))]
      [`(force ,e0)
       `(force ,(w-pul e0))]
      [`(and ,es ...)
       `(and ,@(map w-pul es))]
      [`(or ,es ...)
       `(or ,@(map w-pul es))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,(w-pul e0) ,(w-pul e1) ,(w-pul e2))]
      [`(when ,g ,es ..1)
       `(when ,(w-pul g) ,(w-pul `(begin ,@es)))]
      [`(unless ,g ,es ..1)
       `(unless ,(w-pul g) ,(w-pul `(begin ,@es)))]
      [`(set! ,x ,e0)
       `(set! ,x ,(w-pul e0))]
      [`(call/cc ,e0)
       `(call/cc ,(w-pul e0))]
      [`(let/cc ,x ,e0)
       `(let/cc ,x ,(w-pul e0))]
      [`(guard (,x ,cond-clauses ...) ,es ..1)
       `(guard (,x ,@(map t-cond-cl cond-clauses)) ,(w-pul `(begin ,@es)))]
      [`(apply ,e0 ,e1)
       `(apply ,(w-pul e0) ,(w-pul e1))]
      [`(define (,name ,(? symbol? xs) ... [,dxs ,des] ...) ,bes ..1)
       (define rest (gensym 'rest))
       (define dxs-des (map (lambda (x e0) `[,x ,(w-pul e0)]) dxs des))
       (if (null? dxs-des) `(set! ,name (lambda (,@xs) ,(w-pul `(begin ,@bes))))
           `(set! ,name (lambda (,@xs . ,rest)
                          (letrec* (,@dxs-des)
                                   ,(w-pul `(begin ,@(map (lambda (x) `(unless (null? ,rest) (set! ,x (car ,rest)) (set! ,rest (cdr ,rest)))) dxs)
                                                   ,@bes))))))]
      [(and `(define (,name ,@(list-rest params ... rest)) ,bes ..1) `(define (,name ,@(? cons? all-params)) ,bes0 ..1))
       `(set! ,name (lambda (,@params . ,rest) ,(w-pul `(begin ,@bes))))]
      [`(define ,(cons name params) ,bes ..1)
       `(set! ,name (lambda ,params ,(w-pul `(begin ,@bes))))]
      [`(define ,x ,e0)
       `(set! ,x ,(w-pul e0))]
      [`(match ,e0 ,m-clauses ...)
       (trans `(case ,e0 ,@m-clauses))]
      [`(cond ,clauses ...)
       `(cond ,@(map t-cond-cl clauses))]
      [`(case ,grd ,clauses ...)
       (define (t-case-cl clause)
         (match clause
           [`(else ,es ..1)
            `(else ,(w-pul `(begin ,@es)))]
           [`(,dats ,es ..1)
            `(,dats ,(w-pul `(begin ,@es)))]))
       `(case ,(w-pul grd) ,@(map t-case-cl clauses))]
      [(or (? integer? d) (? string? d) (? boolean? d) (? char? d))
       `(quote ,d)]
      [(? prim? op)
       op]
      [(? symbol? x)
       x]
      [`(quasiquote ,qq)
       (define (t-qq qq count)
         (match qq
           [`(quasiquote ,qq)
            `(list 'quasiquote ,(t-qq qq (add1 count)))]
           [`(,'unquote ,e)
            (if (= count 1) (w-pul e) (list 'list ''unquote (t-qq e (sub1 count))))]
           [(? cons? cell)
            `(cons ,(t-qq (car cell) count) ,(t-qq (cdr cell) count))]
           [(? datum? d)
            `',d]))
       (t-qq qq 1)]
      [`(quote ,(? datum? dat))
       #;(match dat
           [`#(,(? datum? ds) ...)
            `(quote #(,@(map trans ds)))]
           [`(,(? datum? ds) ...)
            `(quote (,@(map trans ds)))]
           [(cons (? datum? d1) (? datum? d2))
            `(quote ,(cons (trans d1) (trans d2)))]
           [else
            `(quote ,dat)])
       `(quote ,dat)]
      [`(,e0 ,es ...)
       `(,(w-pul e0) ,@(map w-pul es))]))

  (define (pul es)
    (define (pul-h e)
      (match e
        [`(begin ,es ..1)
         (pul es)]
        [`(define (,name ,(? symbol? params) ... [,dxs ,des] ...) ,bes ..1)
         `([,name (void)])]
        [(and `(define (,name ,@(list-rest params ... rest)) ,bes ..1) `(define (,name ,@(? cons? all-params)) ,bes0 ..1))
         `([,name (void)])]
        [`(define ,(cons name params) ,bes ..1)
         `([,name (void)])]
        [`(define ,x ,e0)
         `([,x (void)])]
        [else
         '()]))
    (remove-duplicates (foldl (lambda (e acc) (append acc (pul-h e))) '() es)))
  (w-pul e))
