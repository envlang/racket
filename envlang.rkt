#lang racket
(define-syntax-rule (matches? pat ...) (match-lambda [pat #t] ... [else #f]))
(define ((procedure/arity? a) p) (and (procedure? p) (procedure-arity-includes? p a)))
(define v?       (matches? `(\\ env χ ,_) (? hash?) (? string?) (? number?) `(ffi ,(? (procedure/arity? 3)))))
(define e-not-v? (matches? `(@ ,e-f ,e-env ,e-arg) `(thunk ,e) 'env 'χ (? symbol?)))

(define (eval debug? env+χ+redex+k-frames)
  (when debug? (println (third env+χ+redex+k-frames)))
  (define (r debug env+χ+redex+k-frames) (when debug? (displayln debug)) (eval debug? env+χ+redex+k-frames))
  (match env+χ+redex+k-frames
    [`{,E ,X ,(? v? v) ()}
     v]
    ;; Primitive application
    [           `{,E     ,X                (@ (\\ env χ ,e) ,(? v? v-env) (\\ env χ ,e-arg))                                      ,… }
     (r "APP"   `{,v-env (\\ env χ ,e-arg)              ,e                                                                        ,… })]
    [           `{,E     ,X                (@ (ffi ,f)      ,(? v? v-env) (\\ env χ ,e-arg))                                      ,… }
     (r "FFI"   `{,E     ,X                ,(f r v-env `(\\ env χ ,e-arg))                                                        ,… })]
    ;;---------------------------------------------------------------------------------------------------------------------------
    ;; Evaluation of sub-parts of an application    
    [           `{,E     ,X                (@  ,(? e-not-v? e-f)              ,e-env ,e-arg)                                      ,… }
     (r "@-F"   `{,E     ,X                                ,e-f                               ((,E  ,X  (@ _    ,e-env ,e-arg)) . ,…)})]
    [           `{,E     ,X                (@  ,(? v? v-f)       ,(? e-not-v? e-env) ,e-arg)                                      ,… }
     (r "@-ENV" `{,E     ,X                                                  ,e-env           ((,E  ,X  (@ ,v-f _      ,e-arg)) . ,…)})]
    [           `{,E     ,X                (@  ,(? v? v-f) ,(? v? v-env) ,(? e-not-v? e-arg))                                     ,… }
     (r "@-ARG" `{,E     ,X                                                          ,e-arg   ((,E  ,X  (@ ,v-f ,v-env _     )) . ,…)})]
    
    [           `{,E     ,X                   ,(? v? v-f)                                     ((,E′ ,X′ (@ _ ,e-env ,e-arg))    . ,…)}
     (r "K-F"   `{,E′    ,X′                     (@ ,v-f ,e-env ,e-arg)                                                           ,… })]
    [           `{,E     ,X                        ,(? v? v-env)                              ((,E′ ,X′ (@ ,v-f _   ,e-arg))    . ,…)}
     (r "K-ENV" `{,E′    ,X′                     (@ ,v-f ,v-env ,e-arg)                                                           ,… })]
    [           `{,E     ,X                               ,(? v? v-arg)                       ((,E′ ,X′ (@ ,v-f ,v-env _  ))    . ,…)}
     (r "K-ARG" `{,E′    ,X′                     (@ ,v-f ,v-env ,v-arg)                                                           ,… })]
    ;;---------------------------------------------------------------------------------------------------------------------------
    ;; Syntactic sugar
    ;; insertion of #%app at the front of all parentheses that don't start with an @ or \ or ffi or thunk or #%app
    [           `{,E     ,X                (,(and (not '@ '\\ 'ffi 'thunk '#%app) e-f) ,e-arg)                                    ,… }
     (r "#%app" `{,E     ,X                (#%app                         ,e-f  ,e-arg)                                           ,… })]
    [           `{,E     ,X                (#%app                         ,e-f  ,e-arg)                                           ,… }
     (r "@%app" `{,E     ,X                (@ (@ (@ #%get env (\\ env χ "#%app"))
                                                 env (\\ env χ ,e-f))
                                              env (\\ env χ ,e-arg))                                                              ,… })]
    [           `{,E     ,X                (λ ,var-name ,e)                                                                       ,… }
     (r "LAM"   `{,E     ,X                (#%app (#%app λ ,var-name) ,e)                                                         ,… })]
    [           `{,E     ,X                (thunk ,e)                                                                             ,… }
     (r "THUNK" `{,E     ,X                (\\ env χ (@ (\\ env χ ,e) env ,X))                                                    ,… })]
    ;;---------------------------------------------------------------------------------------------------------------------------
    ;; Built-ins and variables
    [           `{,E     ,X                env                                                                                    ,… }
     (r "VAR"   `{,E     ,X                ,E                                                                                     ,… })]
    [           `{,E     ,X                χ                                                                                      ,… }
     (r "VAR"   `{,E     ,X                ,X                                                                                     ,… })]
    [           `{,E     ,X                #%get                                                                                  ,… }
     (r "VAR"   `{,E     ,X                ,(car (hash-ref E "#%get"))                                                            ,… })]
    [           `{,E     ,X                ,(? symbol? var-name)                                                                  ,… }
     (r "VAR"   `{,E     ,X                (@ #%get env (\\ env χ ,(symbol->string var-name)))                                    ,… })]
    ;;---------------------------------------------------------------------------------------------------------------------------
    [other
     `(stuck . ,other)]))

(define unit '(\\ env χ χ))
(define (#%force eval env t)                        (eval "FFI:FORCE" `{,env ,unit (@ ,t ,env ,unit) ()}))
(define (#%get   eval env χ)                        (car (hash-ref env (#%force eval env χ))))
(define (#%push  ev1 env1 χ) `(ffi ,(λ (ev2 env2 v) (hash-update env1 (#%force ev1 env1 χ) (λ (vs) (cons (#%force ev2 env2 v) vs)) '()))))
(define (#%drop  eval env χ)                        (hash-update env (#%force eval env χ) (λ (vs) (cdr vs))))
(define (-#%app  ev1 env1 f) `(ffi ,(λ (ev2 env2 a) `(@ ,(#%force ev1 env1 f) env (\\ env χ ,(#%force ev2 env2 a))))))
(define (#%lam   ev1 env1 a) `(ffi ,(λ (ev2 env2 e)
                                      (let ([astr (match ([`(\\ env χ (? symbol? a)) (symbol->string a)]))])
                                        `(@ capture
                                            env
                                            (\ env χ (@ (\ env χ ,e)
                                                        (@ (@ #%push env ,astr) env χ)
                                                        χ)))))))
(define (#%capture eval E f) `(\ env χ (@ ,f ,E χ)))
(define-syntax-rule (ffis f ...) (make-hash (list (cons (symbol->string 'f) `((ffi ,f))) ...)))
(define initial-env
  (let ([#%app -#%app]) (ffis #%force #%get #%push #%drop #%app)))



(define e-or-v? (or? e-not-v? v?))


(require rackunit predicates)
(define (ev e [debug? #f]) (eval debug? `(,initial-env (\\ env χ "argv") ,e ())))

(check-pred v? '(\\ env χ 1))
(check-pred v? '(\\ env χ (\\ env χ 1)))
(check-pred v? #hash())
(check-pred v? initial-env)
(check-pred v? "foo")
(check-pred v? 1)
(check-pred v? `(ffi ,(lambda (eval env χ) 42)))
(check-pred v? `(ffi ,#%get))
(check-pred v? `(ffi ,#%push))
(check-pred v? `(ffi ,#%drop))
(check-pred e-not-v? '(@ (\\ env χ 1) #hash() 2))
(check-pred (not? v?) '(@ (\\ env χ 1) #hash() 2))
(check-pred (not? e-not-v?) '(\\ env χ 1))
(check-pred (not? e-not-v?) '(\\ env χ (\\ env χ 1)))
(check-pred (not? e-not-v?) #hash())
(check-pred (not? e-not-v?) "foo")
(check-pred (not? e-not-v?) 1)
(check-pred (not? e-not-v?) `(ffi ,(lambda (env χ) 42)))
(check-pred e-or-v? '(\\ env χ 1))
(check-pred e-or-v? '(\\ env χ (\\ env χ 1)))
(check-pred e-or-v? #hash())
(check-pred e-or-v? "foo")
(check-pred e-or-v? 1)
(check-pred e-or-v? `(ffi ,(lambda (eval env χ) 42)))
(check-pred e-or-v? '(@ (\\ env χ 1) #hash() 2))

(check-equal? (ev '(\\ env χ 1)) '(\\ env χ 1))
(check-equal? (ev #hash()) #hash())
(check-equal? (ev "foo") "foo")
(check-equal? (ev 1) 1)
(let ([example-ffi `(ffi ,(lambda (eval env χ) 42))])
  (check-equal? (ev example-ffi) example-ffi))
(check-equal? (ev `(ffi ,#%get)) `(ffi ,#%get))
(check-equal? (ev `(ffi ,#%push)) `(ffi ,#%push))
(check-equal? (ev `(ffi ,#%drop)) `(ffi ,#%drop))
(check-equal? (ev '#%get) `(ffi ,#%get))
(check-equal? (ev '#%push) `(ffi ,#%push))
(check-equal? (ev '#%drop) `(ffi ,#%drop))
;; TODO: test #%get, #%push, pop, FFI
(check-equal? (ev '(@ (\\ env χ 1) #hash() (\\ env χ 2))) 1)
(check-equal? (ev '(@ (\\ env χ 1) env (\\ env χ 2))) 1)
(check-equal? (ev 'env) initial-env)
(check-equal? (ev 'χ) '(\\ env χ "argv"))
(check-equal? (ev '(@ #%force env χ)) '"argv")
(check-equal? (ev '(@ (\\ env χ 1) env (\\ env χ 2))) 1)
(check-equal? (ev '(@ (\\ env χ #%get) env (\\ env χ χ))) `(ffi ,#%get))
(check-equal? (ev '(@ (\\ env χ #%push) env (\\ env χ χ))) `(ffi ,#%push))
(check-equal? (ev '(@ (\\ env χ #%drop) env (\\ env χ χ))) `(ffi ,#%drop))
(check-equal? (ev '(@ #%force env (\\ env χ χ))) unit)
(check-equal? (ev '(@ #%force env (\\ env χ 42))) 42)
(check-equal? (ev '(@ #%force env (\\ env χ (\\ env χ χ)))) '(\\ env χ χ))
(check-equal? (ev '(thunk χ)) '(\\ env χ (@ (\\ env χ χ) env (\\ env χ "argv"))))
(check-equal? (ev '(@ #%force env (thunk (@ #%force env χ)))) "argv")
(check-equal? (ev '(@ #%force env (thunk 3))) 3)
(check-equal? (ev '(#%force 3)) 3)




#;(
;; Primitive application
;; defaults to:
 =>            env=[E],    χ=X               (@     e-f env (\ env χ e-arg))                      …

;; In particular, the sugared λ is just a function
;; defaults to:
 =>            env=[E],    χ=X               (@ capture
                                                env
                                                (\ env χ (@ (\ env χ e)
                                                            (@ (@ #%push env "var-name") env χ)
                                                           χ)))

 CAPTURE       env=[E],    χ=X               (@ capture v-env (\ env χ e))                        …
 =>            env=[E],    χ=X               (\ env χ (@ (λ env χ e) v-env χ))                    …

 FORCE         env=[E],    χ=(\ env χ e-arg) (@ #%force v-env (\ env χ e))                          …
 =>            env=[E],    χ=()              (@ (\ env χ e) v-env dummy)                          …
)












#|
;; Syntax of the language:
;;
;; Plain λ-calculus:
;; x,y,z ::= variable name   Variable
;; e ::= (λ x e)             Abstraction (lambda)
;;    |  (e₁ e₂)             Application
;;    |  x                   variable reference
;;
;; Plain λ-calculus + laziness:
;; e ::= …
;;    |  (#%app e₁ e₂)       Sugar application
;;
;; Translation to λ-calculus
;; (#%app e₁ e₂)          => ((e₁ env) (λ _ e₂))
;;
;; Plain λ-calculus + continuations:
;; e ::= (λ k x e)                                             Abstraction (lambda)
;;    |  (call/prompt stack-frame-name e₁ continuation e₂)     Primitive application
;;    |  x                                                     variable reference
;;    |  (#%app e₁ e₂)                                         Sugar application
;;    |  (#%lam x e)                                           Sugar lambda
;;
;; (#%app e₁ e₂)          => (call/cc (λ (k) (call/prompt "stack frame" e₁ k e₂))

(f e) => (λ kont . (eval-f k=(λ res-f (eval-e k=(λ res-e (res-f res-e k=kont))))))






















;; translation rules
x       => (λ k . k x)
(λ x e) => (λ k . k (λ (k' x) . [[e]] k' ))
(f arg) => (λ k . k ( [[f]] (λ fval . [[arg]] (λ argval . fval k argval) )))

eval k x => k x
eval k (λ x e) => can't reduce further
eval k (f arg) => (eval f) then (eval arg) then (eval k (fval argval))


;; Plain λ-calculus + continuations:
;; e ::= (λ k x e)           Abstraction (lambda)
;;    |  (e₁ k e₂)           Primitive application
;;    |  x                   variable reference
;;    |  (#%app e₁ e₂)       Sugar application is call/cc

eval ((λ k x e) kont param) => e[x := param, k := kont]
eval (#%app f param) => (call/cc f param) => (f current-continuation param)

location of expr    current-continuation
(λ k x _)           k

(_  k e₂)           (λ outer-continuation evaled-f (f k e₂))
(e₁ _ e₂)           ??
(e₁ k  _)           (λ outer-continuation result (e₁ k result))


(#%app _ e₂)       Sugar application is call/cc
(#%app e₁ _)       Sugar application is call/cc

















;; Plain λ-calculus + continuations:
;; e ::= (λ k=x₁ x₂ e)       Abstraction (lambda), takes a continuation
;;    |  (e₁ k=e₂ e₃)        Raw aplication
;;    |  x                   variable reference
;;    |  (#%app e₁ e₂)       Sugar application
;;
;; Evaluation rules:
;; eval env ((λ k=x₂ x₃ e₁) k=e₂ e₃) => eval env[x₂↦e₂][x₃↦e₃] e₁
;; x                                 => env[x]
;; ((#%app e₁ e₂) k=e' e'') =>
;; (e' k=(#%app e₁ e₂) e'') =>
;; (e' k=e'' (#%app e₁ e₂)) => (e₁ k=(λ arg k=? (e' k=e'' arg)) e₂)
;;
;; (#%app f (#%app g x))  => (g k=f x)
;; (f (g (h x)))    => ((g f) (h x)) => (h (g f) x)

;; λk.x => k x
;; λk.λx.e => k (λk λk' (#%app e k))
;;
;; Plain lambda-calculus + first-class environments:
;; "x" ::= "x","y","z"…   String
;; e ::= (λ env arg e)      Abstraction (lambda) which
;;                          * an environment (map from strings to values)
;;                          * takes an argument always named arg which is not added to the env
;;    |  (e env e)          Application
;;    |  env                the env of the innermost lambda containing this expression
;;    |  arg                the arg of the innermost lambda containing this expression
;; prim ::=
;;    |  get                Get variable from environment, type is (→ Environment → String Any)
;;    |  add                Extend environment with new binding, type is (→ Environment String (→ _Environment Any Environment)))
;;
;; Translation to plain lambda-calculus:
;; (λ env arg e)    => (λ arg (λ env e))
;; (e₁ env e₂)      => ((e₁ env) e₂)
;; env              => env
;; arg              => arg
;; get              => primitive "get" from an immutable name↦val mapping (could be implemented in plain lambda-calculus)
;; add              => primitive "add" to an immutable name↦val mapping (could be implemented in plain lambda-calculus)
;;
;; With laziness:
;; (e₁ env e₂)      => ((e₁ env) (λ env (λ _ e₂)))
;;
;; With continuations
;; (e₁ env e₂)      => ((e₁ env) (λ env (λ _ e₂)))
;; (f (g x)) => (g k=f x)
;;
;; With #%app
;; 
|#





















;; "x" ::= "x","y","z"…   String
;; e ::= (-λ -env -arg -k e)      Abstraction (lambda) which takes
;;                                  * an environment            always named -env (not in the -env)
;;                                  * a promise for an argument always named -arg (not in the -env)
;;                                  * a continuation            always named -k   (not in the -env)
;;    |  (v e-env e-arg e-k)      Tail call
;;    |  (v e-env ()    e-k)      Forcing a promise
;;    |  (v ()    e-ret  ())      Calling a continuation
;;    |  -env                     the -env
;;    |  -arg                     the -arg of the innermost lambda
;;    |  -k                       the continuation of the innermost lambda
;;    |  (-get e-env e-str)       Get variable from environment
;;    |  (-add e-env e-str e-val) Extend environment with new binding


#|
(λ -env -arg -k
  ((get -env "1+") (-add -env "foo" 42) -arg -k))

(λ -env -arg -k
  (let (["env2" (-add -env "foo" 42)])
    ((get -env "1+") (get -env "env2") -arg -k)))

(define -lambda '…)
|#



















#;(
 ;; lambda calculus:
 v ::= (λ x e)
    || "str"
    || 0
 e ::= v
    || x
    || (e e)

 ;; reduction:
    redex                         continuation frames
    (((λ x (λ y x)) 1) (inc 1))   _
 =>  ((λ x (λ y x)) 1)            _  (_                 (inc 1))
 =>        (λ y 1)                _  (_                 (inc 1))
 => (      (λ y 1)     (inc 1))   _
 =>                    (inc 1)    _  ((λ y 1)           _      )
 =>                    2          _  ((λ y 1)           _      )
 => (      (λ y 1)     2      )   _
 =>             1                 _

 ;; state of evaluation:
          redex = (v1 v2)
   continuation = (λ result e)
)


#;(
 ;; Using explicit closures:
 v ::= (λ […] x e)
    || "str"
    || 0
 e ::= v
    || (λ ?? x e)
    || x
    || (e e)

    
 ;; Rules:
 rule name     environment   redex              continuation frames
 =>            environment′  redex′             continuation frames′

 APP           [E]           ((λ [E′] x e) v)   …
 =>            [E′,x=v]                 e       …

 CAPTURE       [E]           (λ ??  x e)        …
 =>            [E]           (λ [E] x e)        …

 APP-F         [E]           (e-f e-arg)        …
 =>            [E]            e-f               … E,(_ e-arg)

 APP-ARG       [E]           (v-f e-arg)        …
 =>            [E]                e-arg         … E,(v-f _)

 CONTINUE-F    [E]            v-f               … E′,(_ e-arg)
 =>            [E′]          (v-f e-arg)        …

 CONTINUE-ARG  [E]                v-arg         … E′,(v-f _)       Optimization: [],(v-f _)
 =>            [E′]          (v-f v-arg)        …

 DEREFERENCE   [E,x=v,E′]    x                  …
 =>            [E,x=v,E′]    v                  …
 
 ;; Reduction example:
    env             redex                                 continuation frames                                           rule to use
    [inc=…]         (((λ ?? x (λ ??    y x)) 1) (inc 1))  … […],_                                                       APP-F
 => [inc=…]          ((λ ?? x (λ ??    y x)) 1)           … […],_  [inc=…],(_ (inc 1))                                  APP-F
 => [inc=…]           (λ ?? x (λ ??    y x))              … […],_  [inc=…],(_ (inc 1))  [inc=…],(_ 1)                   CAPTURE
 => [inc=…]           (λ [] x (λ ??    y x))              … […],_  [inc=…],(_ (inc 1))  [inc=…],(_ 1)                   CONTINUE-F
 => [inc=…]          ((λ [] x (λ ??    y x)) 1)           … […],_  [inc=…],(_ (inc 1))                                  APP-ARG
 => [inc=…]                                  1            … […],_  [inc=…],(_ (inc 1))  [inc=…],((λ [] x (λ ?? y x)) _) CONTINUE-ARG
 => [inc=…]          ((λ [] x (λ ??    y x)) 1)           … […],_  [inc=…],(_ (inc 1))                                  APP
 => [inc=…,x=1]               (λ ??    y x)               … […],_  [inc=…],(_ (inc 1))                                  CAPTURE
 => [inc=…,x=1]               (λ [x=1] y x)               … […],_  [inc=…],(_ (inc 1))                                  CONTINUE-F
 => [inc=…]         (         (λ [x=1] y x)     (inc 1))  … […],_                                                       APP-ARG
 => [inc=…]                                     (inc 1)   … […],_  [inc=…],((λ [x=1] y x) _)                            APP-F
 => [inc=…]                                      inc      … […],_  [inc=…],((λ [x=1] y x) _)  [inc=…],(_ 1)             GETVAR
 => [inc=…]                                      …        … […],_  [inc=…],((λ [x=1] y x) _)  [inc=…],(_ 1)             CONTINUE-F
 => [inc=…]                                     (…   1)   … […],_  [inc=…],((λ [x=1] y x) _)                            APP-ARG
 => [inc=…]                                          1    … […],_  [inc=…],((λ [x=1] y x) _)  [inc=…],(… _)             CONTINUE-ARG
 => [inc=…]                                     (…   1)   … […],_  [inc=…],((λ [x=1] y x) _)                            APP
 …
 => [inc=…]                                     2         … […],_  [inc=…],((λ [x=1] y x) _)                            CONTINUE-ARG
 => [inc=…]         (         (λ [x=1] y x)     2      )  … […],_                                                       APP
 => [inc=…,x=1,y=2]                      x                … […],_                                                       GETVAR
 => [inc=…,x=1,y=2]                      2                … […],_                                                       CONTINUE-?
 => […]                                  2                …                                                             …

)


#;(
 ;; Using first-class environments and lazy evaluations:
 ;; λ, env, χ, get, push, #%drop are keywords
 ;; v-env
 v ::= (\ env χ e) ;; open term, expects an env to close the term
    || […]         ;; mapping from names to values
    || "str"
    || 0
    || get
    || push
    || pop
 e ::= v
    || (@ e-f e-env e-arg)


TODO: instead of ad-hoc var-to-string conversion, use a functional env
    
 ;; Rules:
 rule name     environment                   redex                                                continuation frames
 =>            environment′                  redex′                                               continuation frames′

;; Primitive application
 APP           env=[E],    χ=X               (@ (\ env χ e) v-env (\ env χ e-arg))                …
 =>            env=v-env,χ=(\ env χ e-arg)               e                                        …
;;---------------------------------------------------------------------------------------------------------------------------
;; Evaluation of sub-parts of an application
 APP-F         env=[E],    χ=X               (@  e-f e-env e-arg)                                 …
 =>            env=[E],    χ=X                   e-f                                              … env=[E],χ=X,(@ _   e-env e-arg)

 APP-ENV       env=[E],    χ=X               (@ v-f e-env e-arg)                                  …
 =>            env=[E],    χ=X                      e-env                                         … env=[E],χ=X,(@ v-f _     e-arg)

 APP-ARG       env=[E],    χ=X               (@ v-f v-env e-arg)                                  …
 =>            env=[E],    χ=X                            e-arg                                   … env=[E],χ=X,(@ v-f v-env _    )

 CONTINUE-F    env=[E],    χ=X                v-f                                                 … E′,χ=X′,(_   e-env e-arg)
 =>            env=[E′],   χ=X′              (@ v-f e-env e-arg)                                  …

 CONTINUE-ENV  env=[E],    χ=X                    v-env                                           … E′,χ=X′,(v-f _     e-arg)
 =>            env=[E′],   χ=X′              (@ v-f v-env e-arg)                                  …

 CONTINUE-ARG  env=[E],    χ=X                          v-arg                                     … E′,χ=X′,(v-f v-env _    )
 =>            env=[E′],   χ=X′              (@ v-f v-env v-arg)                                  … 

;;---------------------------------------------------------------------------------------------------------------------------
;; Syntactic sugar

;; insertion of #%app at the front of all parentheses that don't start with an @ or \ or #%app
 SUGAR-APP     env=[E],    χ=X               (                         e-f             e-arg )    …
 =>            env=[E],    χ=X               (#%app                    e-f             e-arg )    …
 =>            env=[E],    χ=X                 (@ (@ (@ get env (\ env χ "#%app"))
                                                   env
                                                   (\ env χ e-f))
                                                env
                                                (\ env χ e-arg))                                  …
;; defaults to:
 =>            env=[E],    χ=X               (@     e-f env (\ env χ e-arg))                      …

;; In particular, the sugared λ is just a function
 SUGAR-LAM     env=[E],    χ=X               (λ var-name e)                                       …
 =>            env=[E],    χ=X               (#%app (#%app λ var-name) e)                         …
;; defaults to:
 =>            env=[E],    χ=X               (@ capture
                                                env
                                                (\ env χ (@ (\ env χ e)
                                                            (@ (@ push env "var-name") env χ)
                                                            χ)))

 SUGAR-STR     env=[E],    χ=X                        "str"                                       …
 =>            env=[E],    χ=X               (#%datum "str")                                      …

 SUGAR-NUM     env=[E],    χ=X                        0                                           …
 =>            env=[E],    χ=X               (#%datum 0)                                          …

 SUGAR-VAR     env=[E],    χ=X                        var-name                                    …
 =>            env=[E],    χ=X               (get env var-name)                                   …
;;---------------------------------------------------------------------------------------------------------------------------
 CAPTURE       env=[E],    χ=X               (@ capture v-env (\ env χ e))                        …
 =>            env=[E],    χ=X               (\ env χ (@ (λ env χ e) v-env χ))                    …

 FORCE         env=[E],    χ=(\ env χ e-arg) (@ #%force v-env (\ env χ e))                          …
 =>            env=[E],    χ=()              (@ (\ env χ e) v-env dummy)                          …
)





