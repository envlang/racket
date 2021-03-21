#lang s-exp envlang/tiny

; identity
#;(λ (x) x)
(⧵ env env args args)
#;(|\| #f env args args)

; identity applied to identity
#;((λ (x) x) (λ (x) x))
(@ (⧵ env env args args) env (⧵ env env args args))
#;(|\| #f env args args)

; false a.k.a second-of-two
#;(λ (if-true) (λ (if-false) if-false))
(⧵ env env args (⧵ args env args args))
#;(|\| #f env args (|\| args env args args))

; true a.k.a first-of-two
#;(λ (if-true) (λ (if-false) if-true))
(⧵ env env args (⧵ args env args captured))
#;(|\| #f env args (|\| args env args captured))

; (first-of-two first-of-two second-of-two)
(@ (@ (⧵ env env args (⧵ args env args captured))
      env
      (⧵ env env args (⧵ args env args captured)))
   env
   (⧵ env env args (⧵ args env args args)))
#;(|\| #f env args (|\| args env args captured))

; (second-of-two first-of-two second-of-two)
(@ (@ (⧵ env env args (⧵ args env args args))
      env
      (⧵ env env args (⧵ args env args captured)))
   env
   (⧵ env env args (⧵ args env args args)))
#;(|\| #f env args (|\| args env args args))

; pair
#;(λ (a) (λ (b) (λ (f) ((f a) b))))

;             ↑ a      a ↓      ↑ b        a ↓         f ↑       f ↓        a ↓
#;(⧵ env env args (⧵ args env args (⧵ captured env args (@ (@ args env captured) env BBBBBBBB))))

;             ↑ a      a ↓      ↑ b        b ↓         f ↑       f ↓                       b ↓
#;(⧵ env env args (⧵ args env args (⧵   args   env args (@ (@ args env AAAAAAAA) env captured))))

#;(@ pair
   (⧵ env env args (⧵ args env args captured))
   (⧵ env env args (⧵ args env args args)))

;(@ (@ pair
;      (⧵ env env args (⧵ args env args captured))
;      (⧵ env env args (⧵ args env args args)))
;   (⧵ env env args )

; nil
#;(λ (if-nil) (λ (if-cons) (if-nil 'dummy)))
(⧵ env env args (⧵ args env args (@ captured env (⧵ env env args args))))

; cons
#;(λ (a) (λ (b) (λ (if-cons) (λ (if-nil) (if-cons a b)))))


#;(|\| #f env args (|\| args env args captured))

#;(@ (⧵ #hash() env args
       (list (((λλ x (λλ x 1)) 1) 2)
             (((λλ x (λλ x x)) 1) 2)
             (((λλ x (λλ y y)) 1) 2)
             (((λλ x (λλ y x)) 1) 2)))
   (hash-set env "λλ" (⧵ #hash() env args
                          (⧵ (hash "arg-name" (symbol->string (@ inspect-promise-root env (car (@ force env args))))
                                    "body" (car (cdr (@ force env args)))
                                    "saved-env" env)
                              env
                              args
                              (@ (hash-ref closure "body")
                                 (hash-set (hash-ref closure "saved-env")
                                           (hash-ref closure "arg-name")
                                           (map (make-racket-proc (⧵ #hash() env args
                                                                      (@ force env (car args)))
                                                                  env)
                                                (@ force env args)))
                                 args))))
   (list))