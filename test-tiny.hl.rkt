#lang hyper-literate #:꩜ envlang/tiny

꩜title[#:tag "test-tiny"]{Tests and examples for ꩜racketmodname[envlang/tiny]}

꩜section{Identity}

꩜chunk[<id-λ>
       (λ (x) x)]
꩜chunk[<id>
       (⧵ env env args args)]
꩜chunk[<id-result>
       (⧵ #f env args args)]

꩜section{Dummy value}

꩜chunk[<dummy-λ>
       <id-λ>]

꩜chunk[<dummy>
       <id>]

꩜section{Example: identity applied to identity}

꩜chunk[<id-id-λ>
       (<id-λ> <id-λ>)]
꩜chunk[<id-id>
       (@ <id> env <id>)]
꩜chunk[<id-id-result>
       <id-result>]

꩜section{False}

a.k.a second-of-two

꩜chunk[<false-λ>
       (λ (if-true) (λ (if-false) if-false))]
꩜chunk[<false>
       (⧵ env env args (⧵ args env args args))]
꩜chunk[<false-result>
       (⧵ #f env args (⧵ args env args args))]

꩜section{True}

a.k.a first-of-two

꩜chunk[<true-λ>
       (λ (if-true) (λ (if-false) if-true))]
꩜chunk[<true>
       (⧵ env env args (⧵ args env args captured))]
꩜chunk[<true-result>
       (⧵ #f env args (⧵ args env args captured))]

꩜subsection{Boolean usage example: if true}

꩜chunk[<if-true-example-λ>
       ((<true-λ> <true-λ>) <false-λ>)]
꩜chunk[<if-true-example>
       (@ (@ <true> env <true>) env <false>)]
꩜chunk[<if-false-example-result>
       <true-result>]

꩜subsection{Boolean usage example: if false}

꩜chunk[<if-false-example-λ>
       ((<false-λ> <true-λ>) <false-λ>)]
꩜chunk[<if-false-example>
       (@ (@ <false> env <true>) env <false>)]
꩜chunk[<if-false-example-result>
       <false-result>]

꩜; TODO: take my own red pill / blue pill picture
꩜; ꩜image{/tmp/Two-Buttons.jpg}

꩜section{Pairs}

꩜chunk[<pair-λ>
       (λ (a) (λ (b) (λ (f) ((f a) b))))]
꩜chunk[<pair-failed-attempt-1>
       ;           ↑ a      a ↓      ↑ b        a ↓         f ↑       f ↓        a ↓
       (⧵ env env args (⧵ args env args (⧵ captured env args (@ (@ args env captured) env BBBBBBBB))))]
꩜chunk[<pair-failed-attempt-2>
       ;           ↑ a      a ↓      ↑ b        b ↓         f ↑       f ↓                       b ↓
       (⧵ env env args (⧵ args env args (⧵   args   env args (@ (@ args env AAAAAAAA) env captured))))]

Can't be done because our capture can only close over a single value. We use a primitive:

꩜chunk[<pair>
       ×]

꩜chunk[<pair-result>
       ×]

꩜chunk[<pair-example>
       (@ × <true> <false>)]

꩜chunk[<pair-example-result>
       (⧵ #f env args (@ (@ args env <true-result>) env <false-result>))]

꩜subsection{Fst}

꩜chunk[<fst-λ>
       (λ (p) (p <true-λ>))]

꩜chunk[<fst>
       (⧵ captured env args (@ args env <true>))]

꩜subsection{Snd}

꩜chunk[<snd-λ>
       (λ (p) (p <false-λ>))]

꩜chunk[<snd>
       (⧵ captured env args (@ args env <false>))]

꩜section{Either}

꩜subsection{Left}

꩜chunk[<left-λ>
       (λ (v) (λ (if-left) (λ (if-right) (if-left v))))]
꩜chunk[<left>
       ;          ↑ v     v ↓     ↑ if-left         ↓ if-left    ↓ v        ↑ if-right    ↓ if-left × v
       (⧵ env env args (⧵ args env args (⧵ (@ <pair> args      captured) env args       (@ captured      env <appfv>))))]
꩜chunk[<appfv>
       ;          ↑ f     f ↓      ↑ v       ↓ f       ↓ v
       (⧵ env env args (⧵ args env args (@ captured env args)))]
꩜chunk[<left-result>
       (⧵ #f env args (⧵ args env args (⧵ (@ × args captured) env args (@ captured env (⧵ env env args (⧵ args env args (@ captured env args)))))))]

꩜subsection{Right}

꩜chunk[<right-λ>
       (λ (v) (λ (if-left) (λ (if-right) (if-right v))))]
꩜chunk[<right>
       ;          ↑ v     ↓v↑    if-left    ↓ v ↑       ↑ if-right    ↓ if-right     ↓ v
       (⧵ env env args (⧵ args env args (⧵ captured env args       (@ args       env captured))))]
꩜chunk[<right-result>
       (⧵ #f env args (⧵ args env args (⧵ captured env args (@ args env captured))))]

꩜section{If}

꩜chunk[<if-λ-long>
       (λ (c) (λ (if-true) (λ (if-false) ((c if-true) if-false))))]

When passed a boolean as the first argument (as should be the case), it is equivalent to:

꩜chunk[<if-λ>
       (λ (c) c)]

꩜chunk[<if>
       <id>]

꩜chunk[<if-result>
       <id-result>]

꩜subsection{Match "either"}

꩜chunk[<match-either-λ-long>
       (λ (either) (λ (if-left) (λ (if-right) ((either if-true) if-false))))]

When passed a constructor of the "either" variant as the first argument (as should be the case), it is equivalent to:

꩜chunk[<match-either-λ>
       <id-λ>]

꩜chunk[<match-either>
       <id>]

꩜chunk[<match-either-result>
       <id-result>]

꩜chunk[<match-left-example-λ>
       (((<match-either-λ> (<left-λ> <id-λ>)) <id-λ>) (λ (v) <false-λ>))]
꩜chunk[<match-left-example>
       (@ (@ (@ <match-either> env (@ <left> env <id>)) env <id>) env (⧵ captured env args <false>))]
꩜chunk[<match-left-example-result>
       <id-result>]

꩜chunk[<match-right-example-λ>
       (((<match-either-λ (<right-λ> <id-λ>)) (λ (v) <false-λ>)) <id-λ>)]
꩜chunk[<match-right-example>
       (@ (@ (@ <match-either> env (@ <right> env <id>)) env (⧵ captured env args <false>)) env <id>)]
꩜chunk[<match-right-example-result>
       <id-result>]

꩜section{Null}

꩜chunk[<null-λ>
       (<left-λ> <dummy-λ>)]
꩜chunk[<null>
       (@ <left> env <dummy>)]
꩜chunk[<null-result>
       (⧵ (⧵ #f env args args) env args (⧵ (@ × args captured) env args (@ captured env (⧵ env env args (⧵ args env args (@ captured env args))))))]

꩜section{Cons}

꩜chunk[<cons-λ>
       (λ (a) (λ (b) (<right-λ> (<pair-λ> a b))))]
꩜chunk[<cons>
       (⧵ captured env args (⧵ args env args (@ <right> env (@ <pair> captured args))))]
꩜chunk[<cons-result>
       (⧵ #f env args (⧵ args env args (@ (⧵ env env args (⧵ args env args (⧵ captured env args (@ args env captured)))) env (@ × captured args))))]

꩜subsection{Match "list"}

꩜chunk[<match-null-cons-λ>
       <match-either-λ>]

꩜chunk[<match-null-cons>
       <match-either>]

꩜section{null?}

꩜chunk[<null?-λ>
       (λ (l) (((<match-null-cons-λ> l) (λ (v) <true>)) (λ (v) <false-λ>)))]

꩜chunk[<null?>
       (⧵ captured env args (@ (@ (@ <match-null-cons> env args) env (⧵ captured env args <true>)) env (⧵ captured env args <false>)))]

꩜section{Car}

Since we don't have an error reporting mechanism, we make (car null) = null and (cdr null) = null

꩜chunk[<car-λ>
       (λ (l) (((<match-null-cons-λ> l) <null-λ>) <fst-λ>))]

꩜chunk[<car>
       (⧵ captured env args (@ (@ (@ <match-null-cons> env args) env (⧵ captured env args <null>)) env <fst>))]

꩜chunk[<car-example>
       (@ <car> env (@ (@ <cons> env <true>) env <null>))]

꩜chunk[<car-example-result>
       <true-result>]

꩜chunk[<car-example2>
       (@ <car> env (@ (@ <cons> env <false>) env <null>))]

꩜chunk[<car-example2-result>
       <false-result>]

꩜chunk[<car-example3>
       (@ <car> env <null>)]

꩜chunk[<car-example3-result>
       <null-result>]

꩜section{Cdr}

Since we don't have an error reporting mechanism, we make (car null) = null and (cdr null) = null

꩜chunk[<cdr-λ>
       (λ (l) (((<match-null-cons-λ> l) <null-λ>) <snd-λ>))]

꩜chunk[<cdr>
       (⧵ captured env args (@ (@ (@ <match-null-cons> env args) env (⧵ captured env args <null>)) env <snd>))]

꩜chunk[<cdr-example>
       (@ <cdr> env (@ (@ <cons> env <true>) env <null>))]

꩜chunk[<cdr-example-result>
       <true-result>]

꩜chunk[<cdr-example2>
       (@ <cdr> env (@ (@ <cons> env <true>) env (@ (@ <cons> env <false>) env <null>)))]

꩜chunk[<cdr-example2-result>
       <cdr-example2-list-false-result>]

꩜chunk[<cdr-example2-list-false>
       (@ (@ <cons> env <false>) env <null>)]

꩜chunk[<cdr-example2-list-false-result>
       (⧵ (⧵ #f env args (@ (@ args env <false-result>) env <null-result>))
          env
          args
          (⧵ captured env args (@ args env captured)))]

꩜chunk[<cdr-example3>
       (@ <car> env (@ <cdr> env (@ (@ <cons> env <true>) env (@ (@ <cons> env <false>) env <null>))))]

꩜chunk[<car-example3-result>
       <false-result>]

꩜section{Zero}

꩜chunk[<zero-λ>
       <null-λ>]

꩜chunk[<zero>
       <null>]

꩜section{Not}

꩜chunk[<not-λ>
       (λ (a) (((<if-λ> a) <false>) <true>))]

꩜chunk[<not>
       (⧵ captured env args (@ (@ (@ <if> env args) env <false>) env <true>))]

꩜section{And}

꩜chunk[<and-λ>
       (λ (a) (λ (b) (((<if-λ> a) b) <false-λ>)))]

꩜chunk[<and>
       ;                a       a       b                         a           b
       (⧵ captured env args (⧵ args env args (@ (@ (@ <if> env captured) env args) env <false>)))]

꩜chunk[<and-example-ff>
       (@ (@ <and> env <false>) env <false>)]

꩜chunk[<and-example-ft>
       (@ (@ <and> env <false>) env <true>)]

꩜chunk[<and-example-tf>
       (@ (@ <and> env <true>) env <false>)]

꩜chunk[<and-example-tt>
       (@ (@ <and> env <true>) env <true>)]

꩜section{Or}

꩜chunk[<or-λ>
       (λ (a) (λ (b) (((<if-λ> a) <true>) b)))]

꩜chunk[<or>
       ;                a       a       b                         a                      b
       (⧵ captured env args (⧵ args env args (@ (@ (@ <if> env captured) env <true>) env args)))]

꩜chunk[<or-example-ff>
       (@ (@ <or> env <false>) env <false>)]

꩜chunk[<or-example-ft>
       (@ (@ <or> env <false>) env <true>)]

꩜chunk[<or-example-tf>
       (@ (@ <or> env <true>) env <false>)]

꩜chunk[<or-example-tt>
       (@ (@ <or> env <true>) env <true>)]

꩜section{Equal bools}

꩜chunk[<eqbool-λ>
       (λ (a) (λ (b) (((<if-λ> a) b) (<not-λ> b))))]

꩜chunk[<eqbool>
       (⧵ captured env args (⧵ args env args (@ (@ (@ <if> env captured) env args) env (@ <not> env args))))]

꩜chunk[<eqbool-example-ff>
       (@ (@ <eqbool> env <false>) env <false>)]

꩜chunk[<eqbool-example-ft>
       (@ (@ <eqbool> env <false>) env <true>)]

꩜chunk[<eqbool-example-tf>
       (@ (@ <eqbool> env <true>) env <false>)]

꩜chunk[<eqbool-example-tt>
       (@ (@ <eqbool> env <true>) env <true>)]

꩜section{Z combinator}

꩜chunk[<Z-λ>
       (λ (f) (<half-Z-λ> <half-Z-λ>))]

꩜chunk[<half-Z-λ>
       (λ (x) (f (λ (v) ((x x) v))))]

꩜chunk[<Z>
       ;               ↑ f
       (⧵ captured env args (@ <half-Z> env <half-Z>))]

꩜chunk[<half-Z>
       ;  ↓f↑      ↑ x       ↓ f           ↓x↑      ↑v           ↓ x          ↓ x         ↓ v
       (⧵ args env args (@ captured env (⧵ args env args (@ (@ captured env captured) env args))))]

꩜section{Equality of lists}

꩜chunk[<eqlist-λ>
       (λ (recur)
         (λ (cmp)
           (λ (a)
             (λ (b)
               ((<if-λ> ((<or-λ> (<null?-λ> a)) (<null?-λ> b))
                        (λ (_) ((<and-λ> (<null?-λ> a)) (<null?-λ> b)))
                        (λ (_) ((<if-λ> ((cmp (<car-λ> a)) (<car-λ> b))
                                        (λ (_) (((recur cmp) (<cdr-λ> a)) (<cdr-λ> b)))
                                        (λ (_) <false-λ>))
                                <dummy-λ>)))
                <dummy-λ>)))))]

꩜chunk[<eqlist-noZ>
       ;   recur
       (⧵ captured env args
          ;  recur    cmp
          (⧵ args env args
             ;            recur    cmp       a
             (⧵ (@ <pair> captured args) env args
                ;            recur+cmp  a        b
                (⧵ (@ <pair> captured args) env args
                   ;                                                  a                                           b
                   (@ (@ (@ (@ <if> env (@ (@ <or> env (@ <null?> env (@ <snd> env captured))) env (@ <null?> env args)))
                            ;                                                               a                                           b
                            env         (⧵ captured env args (@ (@ <and> env (@ <null?> env (@ <snd> env captured))) env (@ <null?> env args))))
                         ;                                                                cmp                                                   a
                         env            (⧵ captured env args (@ (@ (@ (@ <if> env (@ (@ (@ <snd> env (@ <fst> env captured)) env (@ <car> env (@ <snd> env captured)))
                                                                                     ;                                                        b
                                                                                     env                                         (@ <car> env args)))
                                                                      env         (⧵ captured env args
                                                                                     ;        recur
                                                                                     (@ (@ (@ (@ <fst> env (@ <fst> env captured))
                                                                                              ;                      cmp
                                                                                              env                    (@ <snd> env (@ <fst> env captured)))
                                                                                           ;                         a
                                                                                           env          (@ <cdr> env (@ <snd> env captured)))
                                                                                        ;                            b
                                                                                        env             (@ <cdr> env args))))
                                                                   env            (⧵ captured env args
                                                                                     <false>))
                                                                env
                                                                args)))
                      env
                      args)))))]

꩜chunk[<eqlist>
       (@ <Z> env <eqlist-noZ>)]

꩜chunk[<eqlist-bool>
       (@ <eqlist> env <eqbool>)]

꩜chunk[<eqlist-list-bool>
       (@ <eqlist> env (@ <eqlist> env <eqbool>))]

꩜chunk[<eqlist-examples>
       ;; These return true
       (@ (@ <eqlist-bool> env <null>) env <null>)
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <true>) env <null>)) env (@ (@ <cons> env <true>) env <null>))
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <false>) env <null>)) env (@ (@ <cons> env <false>) env <null>))
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <false>) env (@ (@ <cons> env <true>) env <null>))) env (@ (@ <cons> env <false>) env (@ (@ <cons> env <true>) env <null>)))
       ;; These return false
       (@ (@ <eqlist-bool> env <null>) env (@ (@ <cons> env <true>) env <null>))
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <true>) env <null>)) env <null>)
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <true>) env (@ (@ <cons> env <true>) env <null>))) env <null>)
       (@ (@ <eqlist-bool> env <null>) env (@ (@ <cons> env <true>) env (@ (@ <cons> env <true>) env <null>)))
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <true>) env <null>)) env (@ (@ <cons> env <false>) env <null>))
       (@ (@ <eqlist-bool> env (@ (@ <cons> env <false>) env (@ (@ <cons> env <true>) env <null>))) env (@ (@ <cons> env <false>) env (@ (@ <cons> env <false>) env <null>)))
       ]

꩜section{Associative lists}

꩜chunk[<assoc-λ>
       (λ (recur)
         (λ (k)
           (λ (l)
             ((if (<null?-λ> l)
                  (λ (_) <false-λ>)
                  ((<if-λ> (<eqlist-list-bool-λ> (<fst-λ> (<car-λ> l)) k)
                           (λ (_) (<snd-λ> (<car-λ> l)))
                           (λ (_) (recur k (<cdr-λ> l))))
                   <dummy-λ>))
              <dummy-λ>))))]

꩜chunk[<assoc-noZ>
       ;               ↑recur
       (⧵ captured env args
          ;  ↓recur↑     ↓k↑
          (⧵ args    env args
             ;            ↓recur   ↓k        ↓l
             (⧵ (@ <pair> captured args) env args
                (@ ;                                ↓l
                 (@ (@ (@ <if> env (@ <null?> env args))
                       env         (⧵ captured env args <false>))
                    env            (⧵ captured env args
                                      ;                                                                                             ↓l          ↓k
                                      (@ (@ (@ (@ <if> env (@ (@ <eqlist-list-bool> env (@ <car> env (@ <car> env args))) env (@ <snd> env captured)))
                                               ;                                                         ↓l
                                               env         (⧵ captured env args(@ <cdr> env (@ <car> env args))))
                                            ;                                        ↓recur                     ↓k                                       ↓l
                                            env            (⧵ captured env args(@ (@ (@ <fst> env captured) env (@ <snd> env captured)) env (@ <cdr> env args))))
                                         env args)))
                 env args))))]
꩜chunk[<assoc>
       (@ <Z> env <assoc-noZ>)]

꩜chunk[<assoc-example-letter-a>
       (@ (@ <cons-bits> env <bit-1>) env (@ (@ <cons-bits> env <bit-1>) env <null-bits>))]
꩜chunk[<assoc-example-letter-b>
       (@ (@ <cons-bits> env <bit-1>) env (@ (@ <cons-bits> env <bit-0>) env <null-bits>))]
꩜chunk[<assoc-example-k>
       (@ (@ <cons-bytes> env <assoc-example-letter-a>) env (@ (@ <cons-bytes> env <assoc-example-letter-b>) env <null-bytes>))]
꩜chunk[<assoc-example-other-k>
       (@ (@ <cons-bytes> env <assoc-example-letter-a>) env (@ (@ <cons-bytes> env <assoc-example-letter-a>) env <null-bytes>))]
꩜chunk[<assoc-example-kv>
       (@ (@ <cons-k-v> env <assoc-example-other-k>) env <false>)]
꩜chunk[<assoc-example-other-kv>
       (@ (@ <cons-k-v> env <assoc-example-k>) env <true>)]
꩜chunk[<assoc-example-env>
       (@ (@ <env-push> env <assoc-example-other-kv>)
          env (@ (@ <env-push> env <assoc-example-kv>)
                 env <env-null>))]
꩜chunk[<assoc-example>
       (@ (@ <env-ref> env <assoc-example-k>)
          env
          <assoc-example-env>)]

꩜section{environment-manipulation functions}

꩜chunk[<bit-0>
       <false>]
꩜chunk[<bit-1>
       <true>]

꩜chunk[<null-bits>
       <null>]
꩜chunk[<cons-bits>
       <cons>]

꩜chunk[<null-bytes>
       <null>]
꩜chunk[<cons-bytes>
       <cons>]

꩜chunk[<cons-k-v>
       <cons>]

꩜chunk[<env-null>
       <null>]
꩜chunk[<env-push>
       <cons>]
꩜chunk[<env-ref>
       <assoc>]

꩜section{todo}

꩜chunk[<TODO>
       (@ (⧵ #hash() env args
             (list (((λλ x (λλ x 1)) 1) 2)
                   (((λλ x (λλ x x)) 1) 2)
                   (((λλ x (λλ y y)) 1) 2)
                   (((λλ x (λλ y x)) 1) 2)))
          (hash-set env "λλ" <todo-lam-impl>)
          (list))]

꩜chunk[<todo-lam-impl>
       (⧵ #hash() env args
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
                args)))]

꩜chunk[<*>
       <assoc-example>]
