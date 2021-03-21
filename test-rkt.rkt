#lang s-exp "envlang-rkt-for-test.rkt"

(require/ffi racket list map car cdr + hash-ref hash-set hash symbol->string)
(check (? (curry equal? (envparam))) env)
(check '0
       0)
(check '(1 2)
       (list 1 2))
(check '3
       (+ 1 2))
(check '4
       (@ (ffi racket *) env (delay (list (delay 2) (delay 2)))))
(check '5
       (@ (\\ env env args 5) env (delay (list (delay 2) (delay 2)))))
(check (hash-table)
       (@ (\\ env env args env) #hash() (delay (list (delay 2) (delay 2)))))
(check (app pproc-repr `(\\ ,(hash-table) env arg (list (delay 2) (delay 2))))
       (@ (\\ env env args args) env (delay (list (delay 2) (delay 2)))))
(check (list (app pproc-repr `(\\ ,(hash-table) env arg 2)) (app pproc-repr `(\\ ,(hash-table) env arg 2)))
       (@ force env (delay (list (delay 2) (delay 2)))))
(check (list (app pproc-repr `(\\ ,(hash-table) env arg 2)) (app pproc-repr `(\\ ,(hash-table) env arg 2)))
       (@ (\\ env env args (@ force env args)) env (delay (list (delay 2) (delay 2)))))
(check (app pproc-repr '(ffi racket +))
       +)
(check (app pproc-repr '(ffi racket *))
       (ffi racket *))
(check (app pproc-repr '#%datum)
       (hash-ref env "#%datum"))
(check (app pproc-repr '(λ x 1))
       (λ x 1))
(check (? (λ (h) (hash-has-key? h "x")))
       ((λ x env) 2))
(check '2
       ((λ xs (car xs)) 2))
(check '3
       (((λ xs (λ xs (car xs))) 2) 3))
(check '(3)
       (((λ xs (λ xs xs)) 2) 3))
(check '(3)
       (((λ xs (λ ys ys)) 2) 3))
(check '(2)
       (((λ xs (λ ys xs)) 2) 3))


#;(λ (.env args) (parameterize ([envparam .env])
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

(check '2
       ((ffi racket procedure-arity) (\\ #hash() env args args)))
(require/ffi "racket-utils.rkt" make-racket-proc)
(check (? (curry equal? (arity-at-least 0)))
       ((ffi racket procedure-arity) (make-racket-proc (\\ #hash() env args args) env)))

((\\ #hash() env args
     (@ force env args))
 x 1)

((\\ #hash() env args
     (map (make-racket-proc (\\ #hash() env args
                                (car args))
                            env)
          (@ force env args)))
 x 1)

((\\ #hash() env args
     (@ inspect-promise-root env (car (@ force env args))))
 x 1)

((\\ #hash() env args
     (car (cdr (@ force env args))))
 x 1)

((\\ #hash() env args
     (car (cdr (@ force env args))))
 x x)

(\\ #hash(("a" . 1)) env args closure)
((\\ #hash(("a" . 1)) env args closure) 2)

(\\ (hash "a" (+ 1 2)) env args closure)
((\\ (hash "a" (+ 1 2)) env args closure) 2)

(((\\ #hash() env args
      (\\ (hash "arg-name" (symbol->string (@ inspect-promise-root env (car (@ force env args))))
                "body" (car (cdr (@ force env args))))
          env
          args
          (@ (hash-ref closure "body")
             (hash-set env
                       (hash-ref closure "arg-name")
                       (map (make-racket-proc (\\ #hash() env args
                                                  (@ force env (car args)))
                                              env)
                            (@ force env args)))
             args)))
  x 1)
 2)


(@ (\\ #hash() env args
       ((λλ x 1)
        2))
   (hash-set env "λλ" (\\ #hash() env args
                          (\\ (hash "arg-name" (symbol->string (@ inspect-promise-root env (car (@ force env args))))
                                    "body" (car (cdr (@ force env args))))
                              env
                              args
                              (@ (hash-ref closure "body")
                                 (hash-set env
                                           (hash-ref closure "arg-name")
                                           (map (make-racket-proc (\\ #hash() env args
                                                                      (@ force env (car args)))
                                                                  env)
                                                (@ force env args)))
                                 args))))
   (list))

(@ (\\ #hash() env args
       ((λλ x 1) 1)
       #;(list (((λλ x (λλ x 1)) 1) 2)
             (((λλ x (λλ x x)) 1) 2)
             (((λλ x (λλ y y)) 1) 2)
             (((λλ x (λλ y x)) 1) 2)))
   (hash-set env "λλ" (\\ #hash() env args
                          (\\ (hash "arg-name" (symbol->string (@ inspect-promise-root env (car (@ force env args))))
                                    "body" (car (cdr (@ force env args)))
                                    "saved-env" env)
                              env
                              args
                              (hash-ref closure "body") #;(@ (hash-ref closure "body")
                                 (hash-set (hash-ref closure "saved-env")
                                           (hash-ref closure "arg-name")
                                           (map (make-racket-proc (\\ #hash() env args
                                                                      (@ force env (car args)))
                                                                  env)
                                                (@ force env args)))
                                 args))))
   (list))