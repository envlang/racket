#lang racket

(require racket/provide
         phc-toolkit/untyped/syntax-parse
         (for-syntax syntax/parse
                     phc-toolkit/untyped/stx))

(provide
 (rename-out [check-for-test check])
 (filtered-out
  (λ (name) (substring name 1))
  (combine-out -begin -@ -⧵ -env -captured -args -× -#%app -#%module-begin -#%top-interaction))) ; -#%datum -#%top -.. -⧵ -ffi -require/ffi -delay -force -inspect-promise-root

;(define cons (gensym 'cons))

;; Printable procedure
(struct pproc (proc repr)
  #:property prop:procedure (struct-field-index proc)
  #:methods gen:custom-write
  [(define write-proc (λ (v port mode)
                        (match mode
                          [#t (write (pproc-repr v) port)]
                          [#f (display (pproc-repr v) port)]
                          [_ (print (pproc-repr v) port 1)])))])

(define-for-syntax (ds stx symbol) (datum->syntax stx symbol stx stx))
;(define-syntax-rule (quasisyntax/top-loc loc stx) #`stx)

(define (init-env-1 bit-0 bit-1 null-bits cons-bits null-bytes cons-bytes cons-k-v env-null env-push env-ref)
  '…)

(define init-env
  (λ (env bit-0)
    (λ (env bit-1)
      (λ (env null-bits)
        (λ (env cons-bits)
          (λ (env null-bytes)
            (λ (env cons-bytes)
              (λ (env cons-k-v)
                (λ (env env-null)
                  (λ (env env-push)
                    (λ (env env-ref)
                      (init-env-1 bit-0 bit-1 null-bits cons-bits null-bytes cons-bytes cons-k-v env-null env-push env-ref))))))))))))

(define capturedparam (make-parameter #f))
(define envparam     (make-parameter init-env))
(define argsparam    (make-parameter #f))

(define-syntax-rule (-begin . rest) (begin . rest))

;; our calculus can only capture one value at a time, the others are supplied by the caller (env
(define -×
  (pproc (λ (a b)
           (pproc (λ (env args)
                    ((args env a) env b))
                  `(⧵ #f env args (@ (@ args env ,a) env ,b))))
         '×))

#;(define envparam
  (make-parameter
   `#hash()
   #;(["#%datum" . ,(pproc (λ (.env args) (parameterize ([envparam .env])
                                            (match (-force (envparam) args) [(list arg) (force arg)])))
                           '#%datum)]
      ["λ"       . ,(pproc (λ (.env args) (parameterize ([envparam .env])
                                            (match (-force (envparam) args)
                                              [(list arg-name-thunk body-thunk)
                                               (define arg-name (-inspect-promise-root (envparam) arg-name-thunk))
                                               (define body     (-inspect-promise-root (envparam) body-thunk))
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
                           'debug)])
   env-guard))

(define-syntax-rule (-delay x)
  (pproc (λ (.env arg)
           (parameterize ([envparam .env])
             x))
         `(⧵ #hash() env arg x)))

(define (-force .env x) (parameterize ([envparam .env]) (x (envparam) '())))
(define (-inspect-promise-root .env x) (match (pproc-repr x) [`(⧵ ,cl env arg ,body) body]))

(define-syntax (-env     stx) (syntax-case stx () [-env     (identifier? #'-env)     #'(envparam)]))
(define-syntax (-captured stx) (syntax-case stx () [-captured (identifier? #'-captured) #'(capturedparam)]))
(define-syntax (-args    stx) (syntax-case stx () [-args    (identifier? #'-args)    #'(argsparam)]))

(define (-@ f .env args) (parameterize ([envparam .env]) (f (envparam) args)))
(define-syntax/parse (-⧵ capture {~and env-stx {~datum env}} {~and args {~datum args}} body)
  #`(let ([saved-capture capture])
      (pproc (λ (e args) (parameterize ([envparam e] [capturedparam saved-capture]) body))
             `(⧵ ,saved-capture env-stx args body))))
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

(define-syntax (-#%top stx)
  (syntax-parse stx
    [(-#%top . var) (quasisyntax/top-loc stx (#%app -.. (#%app envparam) #,(symbol->string (syntax-e #'var))))]))

(define-syntax (-#%app stx)
  (syntax-parse stx
    [(-#%app {~and @ {~datum @}} #:debug dbg f env-expr args) (quasisyntax/top-loc stx (begin (#%app displayln (#%datum dbg)) (#%app -@ f env-expr args)))]
    [(-#%app {~and @ {~datum @}} f env-expr args) (quasisyntax/top-loc stx (#%app -@ f env-expr args))]
    ;[(-#%app f arg ...) (quasisyntax/top-loc stx (-#%app @ f (#%app envparam) (-delay (list (-delay arg) ...))))]
    ))

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