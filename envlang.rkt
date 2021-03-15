#lang racket

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
 ;; λ, env, χ, get, push, drop are keywords
 ;; v-env
 v ::= (\ env χ e) ;; open term, expects an env to close the term
    || […]         ;; mapping from names to values
    || "str"
    || 0
    || get
    || push
    || pop
 e ::= v
    || (@ e e e)


TODO: instead of ad-hoc var-to-string conversion, use a functional env
    
 ;; Rules:
 rule name     environment                   redex                                                continuation frames
 =>            environment′                  redex′                                               continuation frames′

;; Primitive application
 APP           env=E,    χ=X                 (@ (\ env χ e) v-env (\ env () e-arg))               …
 =>            env=v-env,χ=(\ env () e-arg)              e                                        …
;;---------------------------------------------------------------------------------------------------------------------------
;; Evaluation of sub-parts of an application
 APP-F         env=E,    χ=X                 (@  e-f e-env e-arg)                                 …
 =>            env=E,    χ=X                     e-f                                              … [env=E,χ=X],(@ _   e-env e-arg)

 APP-ENV       env=E,    χ=X                 (@ e-f e-env e-arg)                                  …
 =>            env=E,    χ=X                        e-env                                         … [env=E,χ=X],(@ v-f _     e-arg)

 APP-ARG       env=E,    χ=X                 (@ e-f e-env e-arg)                                  …
 =>            env=E,    χ=X                              e-arg                                   … [env=E,χ=X],(@ v-f v-env _    )
;;---------------------------------------------------------------------------------------------------------------------------
;; Syntactic sugar (insertion of #%app)
 SUGAR-APP     env=E,    χ=X                 (#%app                    e-f             e-arg )    …
 =>            env=E′,   χ=X                 (@ (@ (get env "#%app")
                                                   env
                                                   (\ env () e-f))
                                                env
                                                (\ env () e-arg))  …
;; defaults to:
 =>            env=E′,   χ=X                 (@     e-f env (\ env () e-arg))                     …

 SUGAR-LAM     env=E,    χ=X                 (λ var-name e)                                       …
 =>            env=E′,   χ=X                 (#%app (#%app λ var-name) e)                         …
;; defaults to:
 =>            env=E′,   χ=X                 (@ capture
                                                env
                                                (λ env χ (@ (λ env χ e)
                                                            (add env "var-name" χ)
                                                            χ)))
;;---------------------------------------------------------------------------------------------------------------------------
 CAPTURE       env=E,    χ=X                 (@ capture v-env (λ env χ e))                        …
 =>            env=E,    χ=X                 (λ env χ (@ (λ env χ e) v-env χ))                    …

 FORCE         env=E,    χ=(λ env () e-arg)  (@ force v-env (λ env χ e))                                …
 =>            env=E,    χ=()                TODO                                           … [env=E,χ=(λ env () e-arg)],???

 CONTINUE-F    [E]                     v-f             … E′,(_ e-arg)
 =>            [E′]                   (v-f e-arg)      …

 CONTINUE-ARG  [E]                         v-arg       … E′,(v-f _)       Optimization: [],(v-f _)
 =>            [E′]                   (v-f v-arg)      …

)


;; "x" ::= "x","y","z"…   String
;;
;; v ::= (pλ -env e)              promise:      (unit) -> env -> α
;;    |  (kλ -arg e)              continuation: α -> void
;;    |  (cλ -arg e)              closure:      (α -> β)
;;
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












