#lang racket

; By Solomon Kritz

(provide assignment-convert
         alphatize
         anf-convert
         cps-convert)


(require "utils.rkt")

(define (assignment-convert e)
  (define ea (alphatize e))
  
  (define (find-muts muts ea)
    (match ea
      [`(lambda (,xs ...) ,e0)
       (find-muts muts e0)]
      [`(lambda ,x ,e0)
       (find-muts muts e0)]
      [`(let ([,xs ,e0s] ...) ,e1)
       (define temp muts)
       (map (lambda (e0) (set! temp (find-muts temp e0))) e0s)
       (find-muts temp e1)]
      [`(if ,e0 ,e1 ,e2)
       (define temp muts)
       (set! temp (find-muts temp e0))
       (set! temp (find-muts temp e1))
       (find-muts temp e2)]
      [`(apply ,e0 ,e1)
       (define temp muts)
       (set! temp (find-muts temp e0))
       (find-muts temp e1)]
      [`(call/cc ,e0)
       (find-muts muts e0)]
      [`(prim ,op ,es ...)
       (define temp muts)
       (map (lambda (e0) (set! temp (find-muts temp e0))) es)
       temp]
      [`(apply-prim ,op ,e0)
       (find-muts muts e0)]
      [`(set! ,x ,e0)
       (define temp (set-add muts x))
       (find-muts temp e0)]
      [(? symbol? x) 
       muts]
      [`(quote ,(? datum? dat))
       muts]
      [`(,e0 ,es ...)
       (define temp muts)
       (set! temp (find-muts temp e0))
       (map (lambda (e1) (set! temp (find-muts temp e1))) es)
       temp]
      ))
  
  (define mutated-vars (find-muts (set) ea))

  (define (boxify ea)
    (match ea
      [`(let ([,xs ,e0s] ...) ,e1)
       `(let (,@(map (lambda (x e0) (if (set-member? mutated-vars x) `[,x (prim make-vector '1 ,(boxify e0))] `[,x ,(boxify e0)])) xs e0s)) ,(boxify e1))]
      [`(lambda (,xs ...) ,e0)
       `(lambda ,xs (let ,(foldr (lambda (x acc) (if (set-member? mutated-vars x) (cons `[,x (prim make-vector '1 ,x)]  acc) acc)) (list) xs) ,(boxify e0)))]
      [`(lambda ,x ,e0)
       (if (set-member? mutated-vars x)
           `(lambda ,x (let ([,x (prim make-vector '1 ,x)]) ,(boxify e0)))
           `(lambda ,x ,(boxify e0)))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,(boxify e0) ,(boxify e1) ,(boxify e2))]
      [`(apply ,e0 ,e1)
       `(apply ,(boxify e0) ,(boxify e1))]
      [`(call/cc ,e0)
       `(call/cc ,(boxify e0))]
      [`(prim ,op ,es ...)
       `(prim ,op ,@(map boxify es))]
      [`(apply-prim ,op ,e0)
       `(apply-prim ,op ,(boxify e0))]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]
      [`(set! ,x ,e0)
       `(prim vector-set! ,x '0 ,(boxify e0))]
      [(? symbol? x)
       (if (set-member? mutated-vars x)
           `(prim vector-ref ,x '0)
           x)]
      [`(,e0 ,es ...)
       `(,(boxify e0) ,@(map boxify es))]
      ))
  (boxify ea))

(define (alphatize e)
  (define ((rename env) e)
    (match e
      [`(lambda (,xs ...) ,e0)
       (define xs+ (map (lambda (x) (gensym '_)) xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(lambda ,xs+ ,((rename env+) e0))]
      [`(lambda ,x ,e0)
       (define x+ (gensym '_))
       (define env+ (hash-set env x x+))
       `(lambda ,x+ ,((rename env+) e0))]
      [`(let ([,xs ,e0s] ...) ,e1)
       (define xs+ (map (lambda (x) (gensym '_)) xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(let (,@(map (lambda (x+ e0) `[,x+ ,((rename env) e0)]) xs+ e0s)) ,((rename env+) e1))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,((rename env) e0) ,((rename env) e1) ,((rename env) e2))]
      [`(apply ,e0 ,e1)
       `(apply ,((rename env) e0) ,((rename env) e1))]
      [`(call/cc ,e0)
       `(call/cc ,((rename env) e0))]
      [`(prim ,op ,es ...)
       `(prim ,op ,@(map (rename env) es))]
      [`(apply-prim ,op ,e0)
       `(apply-prim ,op ,((rename env) e0))]
      [`(set! ,x ,e0)
       `(set! ,(hash-ref env x) ,((rename env) e0))]
      [(? symbol? x) 
       (hash-ref env x)]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]
      [`(,e0 ,es ...)
       `(,((rename env) e0) ,@(map (rename env) es))]
      ))
  ((rename (hash)) e))

(define (anf-convert e)
  
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                     [(? symbol? x)
                      (k x)]
                     [`(quote ,(? datum? dat))
                      (k `(quote ,dat))]
                     [`(lambda (,xs ...) ,e0)
                      (k `(lambda ,xs ,e0))]
                     [`(lambda ,x ,e0)
                      (k `(lambda ,x ,e0))]
                     [else
                      (define ax (gensym 'a))
                      `(let ([,ax ,anf])
                         ,(k ax))]))))
  
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es) (lambda (ae)
                                 (normalize-aes (cdr es)
                                                (lambda (aes)
                                                  (k `(,ae ,@aes))))))))
  (define (normalize e k)
    (match e
      [`(let () ,e1)
       (normalize e1 k)]
      [`(let ([,x ,e0] . ,rest) ,e1)
       (normalize e0 (lambda (ne1) `(let ([,x ,ne1]) ,(normalize `(let ,rest ,e1) k))))]
      [(? symbol? x)
       (k x)]
      [`(quote ,(? datum? dat))
       (k `(quote ,dat))]
      [`(lambda (,xs ...) ,e0)
       (k `(lambda ,xs ,(anf-convert e0)))]
      [`(lambda ,x ,e0)
       (k `(lambda ,x ,(anf-convert e0)))]
      [`(prim ,op ,es ...)
       (normalize-aes es (lambda (aes) (k `(prim ,op ,@aes))))]
      [`(if ,e0 ,e1 ,e2)
       (normalize-ae e0 (lambda (ae)
                          (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
      [`(apply ,e0 ,e1)
       (normalize-ae e0 (lambda (ae0)
                          (normalize-ae e1 (lambda (ae1)
                                             (k `(apply ,ae0 ,ae1))))))]
      [`(apply-prim ,op ,e0)
       (normalize-ae e0 (lambda (ae) (k `(apply-prim ,op ,ae))))]
      [`(call/cc ,e0)
       (normalize-ae e0 (lambda (ae) (k `(call/cc ,ae))))]
      [`(,es ...)
       (normalize-aes es k)]))
  (normalize e (lambda (x) x)))

(define (cps-convert e)
  (define (T-ae ae)
    (match ae
      [(? symbol? x) x]
      [`(lambda (,xs ...) ,e0)
       (define k (gensym 'k))
       `(lambda (,k ,@xs) ,(T-e e0 k))]
      [`(lambda ,x ,e0)
       (define t (gensym 't))
       (define k (gensym 'k))
       (define v (gensym 'v))
       `(lambda ,t
          (let ([,k (prim car ,t)])
            (let ([,v (prim cdr ,t)])
              ((lambda (,x) ,(T-e e0 k)) ,v))))
       ]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]))
  
  (define (T-e e cae)
    (match e
      [(? symbol? x)
       `(,cae ,x ,x)]
      [`(quote ,(? datum? dat))
       `(,cae '0 (quote ,dat))] 
      [`(lambda . ,rest)
       `(,cae '0 ,(T-ae e))]
      [`(apply ,ae0 ,ae1)
       (define t (gensym 't))
       `(let ([,t (prim cons ,cae ,ae1)])
          (apply ,(T-ae ae0) ,t))]
      [`(call/cc ,ae0)
       (define f (gensym 'f))
       (define k (gensym 'k))
       `((lambda (,k ,f) (,f ,k ,k)) ,cae ,(T-ae ae0))]
      [`(prim ,op ,aes ...)
       (define t (gensym 't))
       `(let ([,t (prim ,op ,@(map T-ae aes))]) (,cae ,t ,t))]
      [`(apply-prim ,op ,ae0)
       (define t (gensym 't))
       `(let ([,t (apply-prim ,op ,(T-ae ae0))]) (,cae ,t ,t))]
      [`(let ([,x ,e0]) ,e1)
       (define _x (gensym '_))
       (T-e e0 `(lambda (,_x ,x) ,(T-e e1 cae)))]
      [`(if ,ae ,e0 ,e1)
       `(if ,ae ,(T-e e0 cae) ,(T-e e1 cae))]
      [`(,aef ,aes ...)
       `(,(T-ae aef) ,cae ,@(map T-ae aes))]))
  (T-e e '(lambda (k x) (let ([_1 (prim halt x)]) (k x)))))
