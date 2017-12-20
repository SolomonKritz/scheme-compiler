#lang racket

; by Solomon Kritz

(provide desugar)
(require "utils.rkt")

(define (desugar-aux e)
  (match e
    
    [`(raise ,e0)
     `(%exception-handler (prim cons ,(desugar-aux e0) '()))]

    [`(guard [,x ,clauses ...] ,body)
     (desugar-aux `(let ([cc (call/cc (lambda (k) k))]) 
                     (if (cons? cc)

                         ((lambda (,x) (cond ,@clauses)) (car cc))
                     
                         (let ([prev-handler %exception-handler])
                           (dynamic-wind
                            (lambda () (set! %exception-handler cc))  
                            (lambda () ,body)         
                            (lambda () (set! %exception-handler prev-handler))
                            )))))]
    
    [`(letrec* ((,xs ,e0s) ...) ,e1)
     (desugar-aux
      `(let ,(map (lambda (x) `(,x (void))) xs)
         (begin ,@(map (lambda (x e0) `(set! ,x ,e0)) xs e0s) ,e1)))] 

    
    [`(let* ([,xs ,e0s] ...) ,e1)
     (if (null? xs) (desugar-aux `(let () ,e1))
         `(let ([,(car xs) ,(desugar-aux (car e0s))])
            ,(desugar-aux `(let* (,@(map (lambda (x e0) `[,x ,e0]) (cdr xs) (cdr e0s))) ,e1))
            ))]

    [`(let ,proc ([,xs ,e0s] ...) ,e1)
     (desugar-aux `(letrec ([,proc (lambda (,@xs) ,e1)]) (,proc ,@e0s)))]
    
    [`(begin ,es ...)
     (desugar-aux `(let* (,@(map (lambda (ex) `[,(gensym 'temp) ,ex]) (reverse (cdr (reverse es))))) ,(last es)))]

    [`(dynamic-wind ,pre ,body ,post)
     `(%dynamic-wind ,(desugar-aux pre) ,(desugar-aux body) ,(desugar-aux post))]
    
    [`(let/cc ,x ,e0)
     (desugar-aux `(call/cc (lambda (,x) ,e0)))]
    
    [`(call/cc ,e0) 
     `(call/cc 
       ,(desugar-aux 
         `(lambda (k) 
            (,e0 (let ([k-stack %wind-stack]) 
                   (lambda (x)  
                     (begin (%do-wind k-stack) 
                            (k x))))))))] 
    [`(letrec ((,xs ,e0s) ...) ,e1)
     (define ts (map gensym xs))
     `(let ,(map (lambda (x) `(,x '())) xs)
        (let ,(map (lambda (x e) `(,x ,(desugar-aux e))) ts e0s)
          ,(desugar-aux
            `(begin
               ,@(map (lambda (x t) `(set! ,x ,t)) xs ts)
               ,e1))))] 
    
    [`(set! ,x ,e0)
     `(set! ,x ,(desugar-aux e0))]

    [`(delay ,e0)
     `(let ([prom (prim make-vector '4 ''())])
        ((lambda (,(gensym '_) ,(gensym '_) ,(gensym '_) ,(gensym '_)) prom) (prim vector-set! prom '0 '"THISISAPROMISE") (prim vector-set! prom '1 '#f)
                                                                             (prim vector-set! prom '2 '0) (prim vector-set! prom '3 (lambda () ,(desugar-aux e0)))))]
    [`(force ,prom)
     `(if (prim vector? ,(desugar-aux prom))
          (if (prim = (prim vector-length ,(desugar-aux prom)) '0) ,(desugar-aux prom)
              (if (prim eqv? '"THISISAPROMISE" (prim vector-ref ,(desugar-aux prom) '0))
                  (if (prim vector-ref ,(desugar-aux prom) '1)
                      (prim vector-ref ,(desugar-aux prom) '2)
                      (let ([_ (prim vector-set! ,(desugar-aux prom) '2 ((prim vector-ref ,(desugar-aux prom) '3)))])
                        ((lambda (_) (prim vector-ref ,(desugar-aux prom) '2)) (prim vector-set! ,(desugar-aux prom) '1 '#t))))
                  ,(desugar-aux prom)))
          ,(desugar-aux prom))]
    
    [`(let ([,xs ,e0s] ...) ,e1)
     `(let (,@(map (lambda (x e0) `[,x ,(desugar-aux e0)]) xs e0s)) ,(desugar-aux e1))]

    [`(when ,guard ,e0)
     `(if ,(desugar-aux guard) ,(desugar-aux e0) (prim void))]
    [`(unless ,guard ,e0)
     `(if ,(desugar-aux guard) (prim void) ,(desugar-aux e0))]
    [`(if ,guard ,b1 ,b2)
     `(if ,(desugar-aux guard) ,(desugar-aux b1) ,(desugar-aux b2))]

    [`(cond ,clauses ...)
     (if (null? clauses) '(prim void)
         (match (car clauses)
           [`(else ,e0)
            `(,@(desugar-aux e0))]
           [`(,guard ,e0)
            `(if ,(desugar-aux guard) ,(desugar-aux e0) ,(desugar-aux `(cond ,@(cdr clauses))))]
           [`(,e0)
            `(,@(desugar-aux e0))]
           ))]

    [`(and) ''#t]
    [`(and ,es ...)
     (match es
       [`(,e0)
        (desugar-aux e0)]
       [`(,e0 . ,rest)
        `(if ,(desugar-aux e0) ,(desugar-aux `(and . ,rest)) '#f)])]

    [`(or)    ''#f]
    [`(or ,es ...)
     (match es
       [`(,e0)
        (desugar-aux e0)]
       [`(,e0 . ,rest)
        (define %TEMPOR (gensym 'TEMPOR))
        (desugar-aux `(let ([,%TEMPOR ,e0])
                        (if ,%TEMPOR ,%TEMPOR (or . ,rest))))])]
    
    [`(case ,esel ,clauses ...)
     (if (null? clauses) '(prim void)
         (match (car clauses)
           [`(else ,e0)
            `(,@(desugar-aux e0))]
           [`(,dats ,e0)
            (if (null? dats) (desugar-aux `(case ,esel ,@(cdr clauses)))
                `(if (prim eq? ',(car dats) ,(desugar-aux esel)) ,(desugar-aux e0) ,(desugar-aux `(case ,esel ,@(cons (cons (cdr dats) (list e0)) (cdr clauses))))))]
           ))]

    [`(lambda (,xs ...) ,e0)
     `(lambda (,@xs) ,(desugar-aux e0))]
    [(and `(lambda ,(list-rest lvp ... pat) ,e0) `(lambda ,(? cons? params) ,e0))
     (desugar-aux `(lambda t (let* (,@(map (lambda (x) `[,x (let ([temp (car t)])(begin (set! t (cdr t)) temp))]) lvp) [,pat t]) ,e0)))]
    [`(lambda ,x ,e0)
     `(lambda ,x ,(desugar-aux e0))]  

    [`(apply ,e0 ,e1)
     `(apply ,(desugar-aux e0) ,(desugar-aux e1))]
    
    [`(,(? prim? op) ,es ...)
     (match op
       ['promise?
        `(if (prim vector? ,(desugar-aux (car es)))
             (if (prim = (prim vector-length ,(desugar-aux (car es))) '0) '#f
                 (if (prim eqv? '"THISISAPROMISE" (prim vector-ref ,(desugar-aux (car es)) '0))
                     '#t
                     '#f))
             '#f)]
       [ else
         `(prim ,op ,@(map desugar-aux es))])]
    
    [(? prim? op)
     (match op
       ['promise?
        '(lambda (p)
           (if (prim vector? p)
               (if (prim = (prim vector-length p) '0) '#f
                   (if (prim eqv? '"THISISAPROMISE" (prim vector-ref p '0))
                       '#t
                       '#f))
               '#f))]
       [else
        `(lambda args (apply-prim ,op args))])]
    
    [`(quote ,(? datum? dat))
     `(quote ,dat)]
    
    [(? symbol? x)
     x]
    
    [`(,e0 . ,e1)
     `(,(desugar-aux e0) ,@(map desugar-aux e1))]
    
    [else '()]))

(define %wind-stack ''()) 

(define %do-wind '(lambda (new)
                    (unless (eq? new %wind-stack)
                      (let ([tail (common-tail new %wind-stack)])
                        (begin
                          (let f ((l %wind-stack))
                            (unless (eq? l tail)
                              (begin
                                (set! %wind-stack (cdr l))
                                ((cdr (car l)))
                                (f (cdr l)))))
                          (let f ([l new])
                            (unless (eq? l tail)
                              (begin
                                (f (cdr l))
                                ((car (car l)))
                                (set! %wind-stack l)))))))))

(define common-tail '(lambda (x y)
                       (let ((lx (length x))
                             (ly (length y)))
                         (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                    [y (if (> ly lx) (drop y (- ly lx)) y)])
                           (if (eq? x y)
                               x
                               (loop (cdr x) (cdr y)))))))

(define %dynamic-wind '(lambda  (pre body post)
                         (begin
                           (pre)
                           (set! %wind-stack (cons (cons pre post) %wind-stack))
                           (let ([v (body)])
                             (begin
                               (set! %wind-stack (cdr %wind-stack))
                               (post)
                               v)))))

(define %exception-handler ''()) 

(define (desugar e)
  (desugar-aux
   `(let* ([%wind-stack ,%wind-stack]
           [common-tail ,common-tail]
           [%do-wind ,%do-wind]
           [%dynamic-wind ,%dynamic-wind]
           [%exception-handler ,%exception-handler]
           )
      ,e)))
