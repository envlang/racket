#lang hyper-literate #:꩜ envlang/rkt

꩜title[#:tag "demo-rkt"]{Tests and examples for ꩜racketmodname[envlang/rkt]}

꩜section{Identity}

꩜chunk[<λ-using-app>
       (\\ #hash() env args
           (\\ (hash-set
                (hash-set
                 (hash-set
                  empty-hash
                  "arg-name" (symbol->string (@ promise-e env (car (@ force env args)))))
                 "body" (car (cdr (@ force env args))))
                "saved-env" env)
               env
               args
               (@ (hash-ref closure "body")
                  (hash-set (hash-ref closure "saved-env")
                            (hash-ref closure "arg-name")
                            (map (envlang->racket (\\ #hash() env args
                                                      (@ force env (car args)))
                                                  env)
                                 (@ force env args)))
                  args)))]

꩜chunk[<λ>
       (\\ closure env args
           (\\ (@ hash-set env
                  (delay (list (delay (@ hash-set env
                                         (delay (list (delay (@ hash-set env
                                                                (delay (list (delay (@ hash-set env
                                                                                       (delay (list (delay empty-hash)
                                                                                                    (delay "arg-name")
                                                                                                    (delay (@ symbol->string env
                                                                                                              (delay (list (delay (@ promise-e env
                                                                                                                                     (@ car env (delay (list (delay (@ force env args)))))))))))))))
                                                                             (delay "body")
                                                                             (delay (@ car env (delay (list (delay (@ cdr env (delay (list (delay (@ force env args))))))))))))))
                                                      (delay "saved-env")
                                                      (delay env)))))
                               (delay "saved-closure")
                               (delay closure))))
               env
               args
               (@ (@ hash-ref env (delay (list (delay closure) (delay "body"))))
                  (@ hash-set env (delay (list (delay (@ hash-ref env (delay (list (delay closure) (delay "saved-env")))))
                                               (delay (@ hash-ref env (delay (list (delay closure) (delay "arg-name")))))
                                               (delay (@ map env (delay (list (delay (@ envlang->racket env (delay (list (delay  (\\ #hash() env args
                                                                                                                                     (@ force env (@ car env (delay (list (delay args)))))))
                                                                                                                         (delay env)))))
                                                                              (delay (@ force env args)))))))))
                  args)))]

꩜chunk[<λ-env>
       (@ hash-set env (delay (list (delay env) (delay "λλ") (delay <λ>))))]

꩜chunk[<λ-app-env>
       (@ hash-set env (delay (list (delay <λ-env>) (delay "envlang#%app") (delay <app>))))]

꩜chunk[<λ-example-low-level-app>
       (@ (\\ #hash() env args
              (@ list env (delay (list (delay (@ (@ (@ λλ env (delay (list (delay x) (delay (@ λλ env (delay (list (delay x) (delay 1)))))))) env (delay (list (delay 1)))) env (delay (list (delay 2)))))
                                       (delay (@ (@ (@ λλ env (delay (list (delay x) (delay (@ λλ env (delay (list (delay x) (delay x)))))))) env (delay (list (delay 1)))) env (delay (list (delay 2)))))
                                       (delay (@ (@ (@ λλ env (delay (list (delay x) (delay (@ λλ env (delay (list (delay y) (delay y)))))))) env (delay (list (delay 1)))) env (delay (list (delay 2)))))
                                       (delay (@ (@ (@ λλ env (delay (list (delay x) (delay (@ λλ env (delay (list (delay y) (delay x)))))))) env (delay (list (delay 1)))) env (delay (list (delay 2)))))))))
          <λ-env>
          #f)]

꩜chunk[<app>
       (\\ closure env args
           (@ (\\
              closure env args
               (@ (@ force env (@ car env (delay (list (delay args)))))
                  env
                  (delay (@ cdr env (delay (list (delay args)))))))
              env
              (@ force env args)))]

꩜chunk[<λ-example>
       (@ (\\ #hash() env args
              (list (((λλ x (λλ x 1)) 1) 2)
                    (((λλ x (λλ x x)) 1) 2)
                    (((λλ x (λλ y y)) 1) 2)
                    (((λλ x (λλ y x)) 1) 2)))
          <λ-app-env>
          #f)]

(list (((λλ x (λλ x 1)) 1) 2)
                    (((λλ x (λλ x x)) 1) 2)
(((λλ x (λλ y y)) 1) 2)
(((λλ x (λλ y x)) 1) 2))

꩜chunk[<let>
       (\\ closure env args
           (@ (\\ (@ hash-set env
                  (delay (list (delay (@ hash-set env
                                         (delay (list (delay (@ hash-set env
                                                                (delay (list (delay (@ hash-set env
                                                                                       (delay (list (delay (@ hash-set env
                                                                                                              (delay (list (delay empty-hash)
                                                                                                                           (delay "arg-name")
                                                                                                                           (delay (@ symbol->string env
                                                                                                                                     (delay (list (delay (@ promise-e env
                                                                                                                                                            (@ car env (delay (list (delay (@ force env args)))))))))))))))
                                                                                                    (delay "value")
                                                                                                    (delay (@ car env (delay (list (delay (@ cdr env (delay (list (delay (@ force env args))))))))))))))
                                                                             (delay "body")
                                                                             (delay (@ car env (delay (list (delay (@ cdr env (delay (list (delay (@ cdr env (delay (list (delay (@ force env args))))))))))))))))))
                                                      (delay "saved-env")
                                                      (delay env)))))
                               (delay "saved-closure")
                               (delay closure))))
               env
               args
               (@ (@ hash-ref env (delay (list (delay closure) (delay "body"))))
                  (@ hash-set env (delay (list (delay (@ hash-ref env (delay (list (delay closure) (delay "saved-env")))))
                                               (delay (@ hash-ref env (delay (list (delay closure) (delay "arg-name")))))
                                               (delay (@ car env (delay (list (delay (@ map env (delay (list (delay (@ envlang->racket env (delay (list (delay  (\\ #hash() env args
                                                                                                                                     (@ force env (@ car env (delay (list (delay args)))))))
                                                                                                                         (delay env)))))
                                                                              (delay (@ force env args)))))))))))))
                  args))
              env
              (delay (list (delay (@ force env (@ car env (delay (list (delay (@ cdr env (delay (list (delay (@ force env args)))))))))))))))]

                  env
                  (hash-ref closure "value"))

꩜chunk[<let-env>
       (@ hash-set env (delay (list (delay env) (delay "let") (delay <let>))))]

꩜chunk[<program>
       (let x 1
         (let x (let x x x)
           x))]

꩜chunk[<program-with-basic-env>
       (@ (\\ #hash() env args
              (@ (\\ #hash() env args
                     <program>)
                 <let-env>
                 #f))
          <λ-app-env>
          #f)]

꩜chunk[<*>
       (begin
         #;<λ-example>
         <program-with-basic-env>)]