#lang racket

; By Solomon Kritz

(require "utils.rkt")

(provide closure-convert
         proc->llvm)

(define (simplify-ae e)
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
      (foldr (lambda (ae xs+wrap)
               (define gx (gensym 'arg))
               (if (symbol? ae)
                   (cons (cons ae (car xs+wrap))
                         (cdr xs+wrap))
                   (cons (cons gx (car xs+wrap))
                         (lambda (e)
                           (match ae
                             [`(lambda ,xs ,body) 
                              `(let ([,gx (lambda ,xs ,(simplify-ae body))])
                                 ,((cdr xs+wrap) e))]
                             [`',dat
                              `(let ([,gx ',dat])
                                 ,((cdr xs+wrap) e))])))))
             (cons '() wrap)
             aes))
    (wrap+ xs))
  (match e
    [`(let ([,x (lambda ,xs ,elam)]) ,e0)
     `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify-ae e0))]

    [`(let ([,x ',dat]) ,e0)
     `(let ([,x ',dat]) ,(simplify-ae e0))]

    [`(let ([,x (prim ,op ,aes ...)]) ,e0)
     (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify-ae e0))))]
    [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
     (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify-ae e0))))]

    [`(if (lambda . ,_) ,et ,ef)
     (simplify-ae et)]
    [`(if '#f ,et ,ef)
     (simplify-ae ef)]
    [`(if ',dat ,et ,ef)
     (simplify-ae et)]
    [`(if ,(? symbol? x) ,et ,ef)
     `(if ,x ,(simplify-ae et) ,(simplify-ae ef))]

    [`(apply ,ae0 ,ae1)
     (wrap-aes (list ae0 ae1) (lambda (aes) `(apply ,@aes)))]
         
    [`(,aes ...)
     (wrap-aes aes (lambda (xs) xs))]))

(define (remove-varargs e) 
  (match e
    [`(let ([,x ',dat]) ,e0)
     `(let ([,x ',dat]) ,(remove-varargs e0))]
    [`(let ([,x (prim ,op ,xs ...)]) ,e0)
     `(let ([,x (prim ,op ,@xs)]) ,(remove-varargs e0))]
    [`(let ([,x (apply-prim ,op ,y)]) ,e0)
     `(let ([,x (apply-prim ,op ,y)]) ,(remove-varargs e0))]
    [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
     (define na (gensym 'na))
     (define is_null (gensym 'is_null))
     (define halt_too_many (gensym 'halt_too_many))
     (define  num_left (length xs))
     (define gx+e
       (foldr (lambda (x gx+e)
                (define gx (gensym 'rvp))
                (define is_cons (gensym 'is_cons))
                (define halt_too_few (gensym 'halt_too_few))
                (cons gx
                      (if (= num_left 1)
                          `(let ([,x (prim car ,gx)])
                             (let ([,(car gx+e) (prim cdr ,gx)])
                               ,(cdr gx+e)))
                          (begin
                            (set! num_left (sub1 num_left))
                            `(let ([,is_cons (prim cons? ,gx)])
                               (if ,is_cons
                                   (let ([,x (prim car ,gx)])
                                     (let ([,(car gx+e) (prim cdr ,gx)])
                                       ,(cdr gx+e)))
                                   (let ([,halt_too_few (prim haltwithtoofewerr)])
                                     (,(car xs) ,halt_too_few))))))))
              (cons na #;(remove-varargs body)
                    `(let ([,is_null (prim null? ,na)])
                       (if ,is_null ,(remove-varargs body)
                           (let ([,halt_too_many (prim haltwithtoomanyerr)])
                             (,(car xs) ,halt_too_many)))))
              xs))
     `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
        ,(remove-varargs e0))]
    [`(let ([,x (lambda ,y ,body)]) ,e0)
     `(let ([,x (lambda (,y) ,(remove-varargs body))])
        ,(remove-varargs e0))]
    [`(if ,x ,e0 ,e1)
     `(if ,x ,(remove-varargs e0) ,(remove-varargs e1))]
    [`(apply ,f ,args)
     `(,f ,args)] 
    [`(,f ,xs ...)
     (define (bld-lst lst old-cell)
       (if (null? lst) `(,f ,old-cell)
           (let ()
             (define new-cell (gensym 'cell))
             `(let ([,new-cell (prim cons ,(last lst) ,old-cell)]) ,(bld-lst (reverse (cdr (reverse lst))) new-cell)))))
     (define nul (gensym 'nul))
     `(let ([,nul '()]) ,(bld-lst xs nul))]
    ))

(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))
  (define (bottom-up e procs)
    (match e
      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (apply-prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x (apply-prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
         (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
         (bottom-up body procs0+))
       (define env-vars (foldl (lambda (x fr) (set-remove fr x))
                               freelam+
                               xs))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                    (cons (+ 1 cnt)
                                          `(let ([,x (env-ref ,envx ,cnt)])
                                             ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,@xs) ,body++) . ,procs1+))]
      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
         (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
         (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)]
      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)]))
  (match-define `(,main-body ,free ,procs) (bottom-up no-varargs-cps '()))
  `((proc (main) ,main-body) . ,procs))

(define (proc->llvm procs)

  (define (put-i64-on-stack name l2)
    (define l1 (string-append name "ptr = alloca i64, align 8\n"))
    (define l3 (string-append "store volatile i64 " name ", i64* " name "ptr, align 8\n"))
    (string-append l1 l2 l3))

  (define (put-i1-on-stack name l2)
    (define l1 (string-append name "ptr = alloca i1, align 8\n"))
    (define l3 (string-append "store volatile i1 " name ", i1* " name "ptr, align 8\n"))
    (string-append l1 l2 l3))
  
  (define (convert-e e)
    (match e
      [`(if ,grd ,et ,ef)
       (define grd-s (foldr string-append (string) (string-split (symbol->string grd) "%")))
       (define cmp (symbol->string (gensym '%cmp)))
       (define then (symbol->string(gensym 'then)))
       (define else (symbol->string (gensym 'else)))
       (define cont (symbol->string (gensym 'cont)))
       (define fal (symbol->string (gensym '%fal)))
       (define l1 (put-i64-on-stack fal (string-append fal " = call i64 @const_init_false()\n")))
       (define l2 (put-i1-on-stack cmp (string-append cmp " = icmp ne i64 %" grd-s ", " fal "\n")))
       (define l3 (string-append "br i1 " cmp ", label %" then ", label %" else "\n\n"))
       (define l4 (string-append then ":\n" (convert-e et) "br label %" cont "\n\n"))
       (define l5 (string-append else ":\n" (convert-e ef) "br label %" cont "\n\n"))
       (define l6 (string-append cont ":\nret i64 0\n"))
       (string-append l1 l2 l3 l4 l5 l6)]
      [`(let ([,r (apply-prim ,op ,x)]) ,e0)
       (define r-s (string-append "%" (foldr string-append (string) (string-split (symbol->string r) "%"))))
       (define x-s (foldr string-append (string) (string-split (symbol->string x) "%")))
       (put-i64-on-stack r-s (string-append r-s " = call i64 @" (prim-applyname op) "(i64 %" x-s ")\n" (convert-e e0)))]
      [`(let ([,r (env-ref ,env ,(? natural? ind))]) ,e0)
       (define r-s (string-append "%" (foldr string-append (string) (string-split (symbol->string r) "%"))))
       (define env-s (foldr string-append (string) (string-split (symbol->string env) "%")))
       (define vind (symbol->string (gensym '%vind)))
       (define l1 (put-i64-on-stack vind (string-append vind " = call i64 @const_init_int(i64 " (number->string (sub1 ind)) ")\n")))
       (define l2 (put-i64-on-stack r-s (string-append r-s " = call i64 @prim_vector_45ref(i64 %" env-s ", i64 " vind ")\n")))
       (string-append l1 l2 (convert-e e0))]
      [`(clo-app ,clo-name ,arg-list)
       (define clo-name-s (foldr string-append (string) (string-split (symbol->string clo-name) "%")))
       (define arg-list-s (foldr string-append (string) (string-split (symbol->string arg-list) "%")))
       (define fp-int (symbol->string (gensym '%fp-int)))
       (define fp (symbol->string (gensym '%fp)))
       (define tenv (symbol->string (gensym '%tenv)))
       (define func (symbol->string (gensym '%func)))
       (define res (symbol->string (gensym '%res)))
       (define l1 (put-i64-on-stack fp-int (string-append fp-int " = call i64 @clo_code(i64 %" clo-name-s ")\n")))
       (define l2 (string-append fp " = alloca i64 (i64, i64)*, align 8\n"))
       (define l3 (string-append func " = inttoptr i64 " fp-int " to i64 (i64, i64)*\n"))
       (define l4 (string-append "store volatile i64 (i64, i64)* " func ", i64 (i64, i64)** " fp ", align 8\n"))
       (define l5 (put-i64-on-stack tenv (string-append tenv " = call i64 @clo_env(i64 %" clo-name-s ")\n")))
       (define l6 (string-append res " = musttail call fastcc i64 " func "(i64 " tenv ", i64 %" arg-list-s ")\n"))
       (define l7 (string-append "ret i64 " res "\n"))
       (string-append l1 l2 l3 l4 l5 l6 l7)]
      [`(let ([,clo-name (make-closure ,func-name ,env-vars ...)]) ,e0)
       (define clo-name-s (string-append "%" (foldr string-append (string) (string-split (symbol->string clo-name) "%"))))
       (define func-name-s (foldr string-append (string) (string-split (symbol->string func-name) "%")))
       (define nul (symbol->string (gensym '%nul)))
       (define len (symbol->string (gensym '%len)))
       (define tenv (symbol->string (gensym '%tenv)))
       (define func (symbol->string (gensym '%func)))
       (define fp (symbol->string (gensym '%fp)))
       (define func-int (symbol->string (gensym '%func-int)))
       (define l1 (put-i64-on-stack nul (string-append nul " = call i64 @const_init_null()\n")))
       (define l2 (put-i64-on-stack len (string-append len " = call i64 @const_init_int(i64 " (number->string (length env-vars)) ")\n")))
       (define l3 (put-i64-on-stack tenv (string-append tenv " = call i64 @prim_make_45vector(i64 " len ", i64 " nul ")\n")))
       (define index '0)
       (define setup-env (foldl (lambda (var acc)
                                  (let ()
                                    (define vind (symbol->string (gensym '%vind)))
                                    (define var-s (foldr string-append (string) (string-split (symbol->string var) "%")))
                                    (define sel1 (put-i64-on-stack vind (string-append vind " = call i64 @const_init_int(i64 " (number->string index) ")\n")))
                                    (set! index (add1 index))
                                    (define sel2  (string-append "call i64 @prim_vector_45set_33(i64 " tenv ", i64 " vind ", i64 %" var-s ")\n"))
                                    (string-append acc sel1 sel2)
                                    )) (string) env-vars))
       (define l4 (string-append fp " = alloca i64 (i64, i64)*, align 8\n"))
       (define l5 (string-append "store volatile i64 (i64, i64)* @" func-name-s ", i64 (i64, i64)** " fp ", align 8\n"))
       (define l6 (string-append (string-append func " = load i64 (i64, i64)*, i64 (i64, i64)** " fp ", align 8\n")))
       (define l7 (put-i64-on-stack func-int (string-append func-int " = ptrtoint i64 (i64, i64)* " func " to i64\n")))
       (define l8 (put-i64-on-stack clo-name-s (string-append clo-name-s " = call i64 @clo_create(i64 " func-int ", i64 " tenv ")\n")))
       (string-append l1 l2 l3 setup-env l4 l5 l6 l7 l8 (convert-e e0))]
      [`(let ([,r (prim ,op ,xs ...)]) ,e0)
       (define r-s (string-append "%" (foldr string-append (string) (string-split (symbol->string r) "%"))))
       (define %r (string-append r-s " = call i64 @" (prim-name op) "("))
       (define params (foldl (lambda (x acc) (let () (define x-s (foldr string-append (string) (string-split (symbol->string x) "%")))
                                               (string-append acc "i64 %" x-s ", "))) (string) xs))
       (unless (null? xs) (set! params (substring params '0 (- (string-length params) '2))))
       (string-append (put-i64-on-stack r-s (string-append %r params ")\n")) (convert-e e0))
       ]
      [`(let ([,r ',(? datum? dat)]) ,e0)
       (define r-s (string-append "%" (foldr string-append (string) (string-split (symbol->string r) "%"))))
       (define lines
         (match dat
           [(? null?)
            (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_null()\n"))]
           [(? integer?)
            (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_int(i64 " (number->string dat) ")\n"))]
           [(? boolean?)
            (if dat 
                (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_true()\n"))
                (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_false()\n")))]
           [(? char?)
            (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_char(i8 " (number->string (char->integer dat)) ")\n"))]
           [(? string?)
            (define str-name (symbol->string (gensym '@.str.)))
            (define str-ptr (symbol->string(gensym '%str-ptr)))
            (define str (symbol->string(gensym '%str)))
            (define str-size+1 (number->string (add1 (string-length dat))))
            (set! strs+syms
                  (string-append strs+syms
                                 str-name " = private unnamed_addr constant [" str-size+1 " x i8] c\"" dat "\\00\", align 8\n"))
            (define l1 (string-append str-ptr " = alloca i8*, align 8\n"))
            (define l2 (string-append "store volatile i8* getelementptr inbounds ([" str-size+1 " x i8], [" str-size+1 " x i8]* " str-name ", i64 0, i64 0), i8** " str-ptr ", align 8\n"))
            (define l3 (string-append str " = load i8*, i8** "str-ptr ", align 8\n"))
            (define l4 (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_string(i8* " str ")\n")))
            (string-append l1 l2 l3 l4)]
           [(? symbol?)
            (define str-name (symbol->string (gensym '@.str.)))
            (define str-ptr (symbol->string(gensym '%str-ptr)))
            (define str (symbol->string(gensym '%str)))
            (define str-size+1 (number->string (add1 (string-length (symbol->string dat)))))
            (set! strs+syms
                  (string-append strs+syms
                                 str-name " = private unnamed_addr constant [" str-size+1 " x i8] c\"" (symbol->string dat) "\\00\", align 8\n"))
            (define l1 (string-append str-ptr " = alloca i8*, align 8\n"))
            (define l2 (string-append "store volatile i8* getelementptr inbounds ([" str-size+1 " x i8], [" str-size+1 " x i8]* " str-name ", i64 0, i64 0), i8** " str-ptr ", align 8\n"))
            (define l3 (string-append str " = load i8*, i8** "str-ptr ", align 8\n"))
            (define l4 (put-i64-on-stack r-s (string-append r-s " = call i64 @const_init_symbol(i8* " str ")\n")))
            (string-append l1 l2 l3 l4)]
           ))
       (string-append lines (convert-e e0))]))
  (define (convert-p p)
    (match p
      [`(proc (main) ,e0)
       (string-append  "define fastcc i64 @main() {\n" (convert-e e0) "ret i64 0\n}\n\n")]
      [`(proc (,name ,env ,args) ,e0)
       (string-append "define fastcc i64 @" (symbol->string name) "(i64 %" (symbol->string env) ", i64 %" (symbol->string args) ") {\n"(convert-e e0) "}\n\n")]))
  (define strs+syms (string))
  (define code (foldl (lambda (p acc) (string-append acc (convert-p p))) (string) procs))
  (set! code (foldr string-append (string) (string-split code ">")))
  (set! code (foldr string-append (string) (string-split code "?")))
  (set! code (foldr string-append (string) (string-split code #px"\\S= ")))
  (set! code (foldr string-append (string) (string-split code "/")))
  (set! code (foldr string-append (string) (string-split code "+")))
  (string-append strs+syms "\n" code))
