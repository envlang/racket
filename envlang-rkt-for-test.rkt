#lang racket

(require racket/provide
         phc-toolkit/untyped/syntax-parse
         (for-syntax syntax/parse
                     phc-toolkit/untyped/stx))

(provide
 (rename-out [check-for-test check])
 (filtered-out
  (λ (name) (substring name 1))
  (combine-out -#%datum -#%top -#%app -#%module-begin -#%top-interaction -env -.. -@ -\\ -ffi #;-require/ffi -list -delay -force -closure -begin)))

(define-syntax-rule (-begin . rest) (begin . rest))

;; Printable procedure
(struct pproc (proc repr)
  #:property prop:procedure (struct-field-index proc)
  #:methods gen:custom-write
  [(define write-proc (λ (v port mode)
                        (match mode
                          [#t (display "#;pproc:" port) (write (pproc-repr v) port)]
                          [#f (display "#;pproc:" port) (display (pproc-repr v) port)]
                          [_  (display "#;pproc:" port) (print (pproc-repr v) port 1)])))])

(define-for-syntax (ds stx symbol) (datum->syntax stx symbol stx stx))
(define-syntax-rule (quasisyntax/top-loc loc stx) #`stx)

(define -promise-e
  (pproc (λ (.env x) (match (pproc-repr x) [`(\\ ,cl env arg ,body) body]))
         'promise-e))

(define -envlang->racket
  (pproc (λ (.env args)
           (parameterize ([envparam .env]) 
             (let* ([forced-args (map (curry -force (envparam)) (-force (envparam) args))]
                    [f (car forced-args)]
                    [captured-env (cadr forced-args)])
               (λ args (f captured-env args)))))
         'envlang->racket))

(define/contract (env-guard new-env)
  (-> hash? hash?)
  (begin #;(println new-env) new-env))
(define closureparam
  (make-parameter #hash()
                  env-guard))
(define envparam
  (make-parameter
   `#hash(["#%datum" . ,(pproc (λ (.env args) (parameterize ([envparam .env])
                                                (match (-force (envparam) args) [(list arg) (force arg)])))
                               '#%datum)]
          ["λ"       . ,(pproc (λ (.env args) (parameterize ([envparam .env])
                                                (match (-force (envparam) args)
                                                  [(list arg-name-thunk body-thunk)
                                                   (define arg-name (-promise-e (envparam) arg-name-thunk))
                                                   (define body     (-promise-e (envparam) body-thunk))
                                                   (let ([saved-env (envparam)])
                                                     (pproc (λ (.env args)
                                                              (parameterize ([envparam saved-env])
                                                                (parameterize ([envparam (hash-set (envparam)
                                                                                                   (symbol->string arg-name)
                                                                                                   (map (curry -force (envparam)) (-force (envparam) args)))])
                                                                  (-@ body-thunk (envparam) args))))
                                                            `(λ ,arg-name ,body)))])))
                               'λ)]
          ["debug"   . ,(pproc (λ (.env arg)
                                 (parameterize ([envparam .env])
                                   (displayln (list (envparam) arg))
                                   (displayln (-force (envparam) arg))
                                   '()))
                               'debug)]
          ["symbol->string" . ,(-ffi racket symbol->string)]
          ["envlang->racket" . ,-envlang->racket]
          ["hash-set" . ,(-ffi racket hash-set)]
          ["hash-ref" . ,(-ffi racket hash-ref)]
          ["car" . ,(-ffi racket car)]
          ["cdr" . ,(-ffi racket cdr)]
          ["map" . ,(-ffi racket map)]
          ["empty-hash" . #hash()]
          ["promise-e" . ,-promise-e])
   env-guard))

(define-syntax-rule (-delay x)
  (pproc (λ (.env arg)
           (parameterize ([envparam .env])
             x))
         `(\\ #hash() env arg x)))

(define (-force .env x) (parameterize ([envparam .env]) (x (envparam) '())))
(define-syntax (-env stx) (syntax-case stx () [-env (identifier? #'-env) #'(envparam)]))
(define-syntax (-closure stx) (syntax-case stx () [-closure (identifier? #'-closure) #'(closureparam)]))

(define (-@ f .env args) (parameterize ([envparam .env]) (f (envparam) args)))
(define-syntax/parse (-\\ cl {~and env-stx {~datum env}} {~and args {~datum args}} body)
  #`(let ([saved-cl cl])
      (pproc (λ (e args) (parameterize ([envparam e] [closureparam saved-cl]) body))
             `(\\ ,saved-cl env-stx args body))))
(define-syntax/parse (-ffi lib f)
  (quasisyntax/top-loc stx
                       (pproc (λ (.env args)
                                (parameterize ([envparam .env]) 
                                  (apply (let () (local-require (only-in lib f)) f)
                                         (map (curry -force (envparam)) (-force (envparam) args)))))
                              '(ffi lib f))))
(define-syntax/parse (-require/ffi lib f ...)
  (quasisyntax/top-loc stx
                       (begin (define f (-ffi lib f))
                              ...)))
(define -.. hash-ref)

(define-syntax (-list stx)
  (syntax-case stx ()
    [(-list . args) #'(#%app list . args)]
    [-list (identifier? #'-list) #'(pproc (λ (.env args)
                                            (parameterize ([envparam .env]) 
                                              (apply (let () (local-require (only-in racket list)) list)
                                                     (map (curry -force (envparam)) (-force (envparam) args)))))
                                          '(ffi racket list f))]))

(define-syntax (-#%top stx)
  (syntax-parse stx
    [(-#%top . var) (quasisyntax/top-loc stx (#%app -.. (#%app envparam) #,(symbol->string (syntax-e #'var))))]))

(define (debug)
  (displayln "lalal")
  (displayln (closureparam))
  (displayln (envparam))
  (displayln ""))

(define-syntax (-#%app stx)
  (syntax-parse stx
    [(-#%app {~and @ {~datum @}} f env-expr args) (quasisyntax/top-loc stx (#%app -@ f env-expr args))]
    [(-#%app f arg ...) (quasisyntax/top-loc stx (-#%app @ (#%app -.. (#%app envparam) #,(symbol->string (syntax-e #'envlang#%app))) (#%app envparam) (-delay (-list (-delay f) (-delay arg) ...))))]))

(define-syntax/parse (-#%datum . d) (quasisyntax/top-loc stx (#%app -@ (-#%top . #%datum) (#%app envparam) (-delay (#%datum d)))))
(define-syntax-rule (-#%module-begin . body) (#%module-begin . body))
(define-syntax-rule (-#%top-interaction . body) (#%top-interaction . body))
        
(require rackunit)
(define-syntax/parse (check-for-test expected-pattern actual)
  (quasisyntax/top-loc
   stx
   (check-pred (match-lambda
                 [#,(datum->syntax #'here (syntax->datum #'expected-pattern))
                  #t])
               actual)))