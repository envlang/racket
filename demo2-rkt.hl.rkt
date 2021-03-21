#lang hyper-literate #:꩜ envlang/rkt

꩜title[#:tag "racketfest"]{Envlang @ racketfest}

꩜section{Use cases for macros}

꩜subsection{Environment manipulation}

Adding bindings to the environment, getting bindings from the environment:

꩜chunk[<use-case-bindings>
       (let (var val) body)             ;; env += {var = val}
       (define-struct name (field ...)) ;; env += {"$name-$field" = accessor-fn} …
       (aif condition if-true if-false) ;; env += {it = condition}
       (match v [(cons a b) body])      ;; env += {a = (car v)} {b = (cdr v)}
       ]

꩜subsection{Control flow}

Changing the order of execution:

꩜chunk[<use-case-order>
       (if condition if-true if-false)
       ;; can be expressed as:
       (force (if condition
                  (λ () if-true)
                  (λ () if-false)))

       (match v ([null if-null] [(cons a b) if-cons]))
       ;; can be expressed as:
       (force (if (null? v)
                  (λ () if-null)
                  (λ () (let ([a (car v)] [b (cdr v)]) if-cons))))
       
       (for/list ([x (in-list l)]) body)
       ;; can be expressed as
       (map (λ (x) body) l)
       ]
꩜subsection{Syntactic sugar}

꩜chunk[<use-case-syntactic-sugar>
       (1 + 2 * 3) ;; infix
       (let x = 3 in (+ x 1))
       (for/list x in (list 1 2 3) (+ x 1))
       (let x:int = 3 in (+ x 1))]

꩜subsection{Optimisations}

Optimisations are semantics-preserving compile-time transformations of the program.

꩜chunk[<use-case-optimisations>
       pre-calculated hash table
       loop unrolling
       …]

꩜subsection{Code analysis}

Tracking and propagating annotations on the code:

꩜chunk[<use-case-annotations>
       typed/racket
       source locations
       tooltips]

꩜section{Overview of the semantics}

꩜chunk[<promise>
       (f arg ...)
       ;; is sugar for:
       (@ f env (⧵ (env) arg) ...)]

꩜chunk[<variables>
       x
       ;; is sugar for:
       (hash-ref env x)]

꩜section{First-class solutions}

Adding bindings to the environment, getting bindings from the environment:

꩜subsection{Environment manipulation}

User-defined let:

꩜chunk[<my-let>
       (⧵ outer-env (var val body)
          ;; evaluate body in outer env + var=val
          (force (hash-set outer-env
                           ;; var name
                           (promise->string var)
                           ;; evaluate val in outer env
                           (force outer-env val))
                 body))]

User-defined let with different order for the arguments:

꩜chunk[<use-return+where>
       (return (+ x 1)
               where x = 123)]

꩜chunk[<return+where>
       (⧵ outer-env (body kw-where var kw-= val)
          (assert (string=? (promise->string kw-where) "where"))
          (assert (string=? (promise->string kw-=)     "="))
          (@ my-let outer-env var val body))]

꩜subsection{Control flow}

꩜chunk[<my-if>
       (⧵ outer-env (condition if-true if-false)
          (force env ((force env condition) if-true if-false)))]

꩜subsection{Syntactic sugar}

꩜subsubsection{Identifiers with different meanings}

Bindings in the environment point to a table associating
meanings to values. See ꩜racketmodname[polysemy].

꩜chunk[<variables>
       x
       ;; becomes sugar for:
       (hash-ref (hash-ref env x) "variable")]

꩜racket[in] keyword used in different contexts:

꩜chunk[<let-in-usage>
       (let x = 3 in (+ x 1))]

꩜chunk[<let-in>
       (⧵ outer-env (var kw-= val kw-in body)
          (assert (equal? (hash-ref (hash-ref env (promise->string kw-=))
                                    "let-in keyword")
                          let-in-=))
          (assert (equal? (hash-ref (hash-ref env (promise->string kw-in))
                                    "let-in keyword")
                          let-in-in))
          (@ my-let outer-env var val body))]

꩜chunk[<for-in-usage>
       (for/list x in (list 1 2 3) (+ x 1))]

꩜chunk[<for-in>
       (⧵ outer-env (var kw-in lst body)
          (assert (equal? (hash-ref (hash-ref env (promise->string kw-in))
                                    "for keyword")
                          for-in))
          (@ map outer-env  var val body))]

It's easy to rename just the ꩜racket["let-in keyword"] part
without renaming the ꩜racket["for keyword"] part.

꩜subsubsection{Extra parentheses}

꩜chunk[<use-let-paren>
       (let [x 2]
         (+ x 1))]

꩜chunk[<let-paren>
       (⧵ outer-env (binding body)
          (let varval (force (hash-set "#%app" cons) binding)
            (@ my-let outer-env (car varval) (cadr varval) body)))]

꩜subsubsection{Infix}

꩜chunk[<example-infix>
       (1 + 2 * 3)]

Needs external support in the language (or overloading
꩜racket[#%app]). WIP prototype using
꩜link["http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf" "mixfix"]
on ꩜link["https://repl.it/@envlang/env"]{repl.it} and
꩜link["https://github.com/envlang/env"]{github}.

꩜subsubsection{Manipulating identifiers}

꩜chunk[<example-postfix-ids>
       (let x:int = 3 in (+ x 1))]

꩜chunk[<postfix-ids>
       (⧵ outer-env (var kw-= val kw-in body)
          (let ([forced-val (force outer-env val)])
            (when (ends-with (promise->string var) ":int")
              (assert int? forced-val))
            (@ my-let outer-env var val body)))]

꩜section{Compile-time transformations}

Wrap parts to be evaluated at compile-time, the wrapper acts
like ꩜racket[unquote] where the whole program is in a
꩜racket[quasiquote].

꩜chunk[<compile-time-proposal>
       (run-time
        (let ([x (compile-time (+ 1 2 3))])
          (* x x)))]

꩜chunk[<compile-time-proposal-equivalent>
       `(let ([x ,(+ 1 2 3)])
          (* x x))]

Semantics-preserving: removing the ꩜racket[run-time] and
꩜racket[compile-time] markers must give an equivalent
program.

꩜section{Code analysis}

꩜subsection{Type checking}

These environment manipulations can be modeled with row types:

꩜chunk[<row-type-example>
       (λ (x : (struct [foo : int] [bar : string] . ρ))
         : (struct [foo : int] [quux : int] . ρ)
         (x without .bar
            with .quux = (+ x.foo (string->int x.bar))))]


꩜subsection{Implemented within the language}

… to be explored?

꩜section{Example use}

꩜chunk[<program>
       (my-let x 3
         (let-paren [x 3]
           (let-postfix x:int = 3 in
             (return (for/list z in (compile-time (list 1 2 3))
                       (+ z y))
                     where y = (+ 1 x)))))]


꩜chunk[<env+program>
       (let* ([my-let <my-let>]
              [return <return+where>]
              [my-if <my-if>]
              [let-paren <let-paren>]
              [let-postfix <postfix-ids>]
              )
         <program>)]

꩜chunk[<*>
       ;;<env+program>
       ]
