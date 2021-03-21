#lang scribble/manual

@title{envlang: an experimental language with first-class environments}

@author[@author+email["Suzanne Soy" "racket@suzanne.soy"]]

@defmodule[envlang/rkt]

An implementation which "escapes" to the Racket library for a certain number of basic building blocks

@racket[@#,hash-lang[] @#,racketmodname[s-exp] @#,racketmodname[envlang/rkt]]

See @racketmodname[test-rkt] for examples

@defmodule[envlang/tiny]

An implementation which starts with a tiny set of
primitives, and builds the basic building blocks using
those. The building blocks (lists, strings, associative
tables) are built in a naive and inefficient way.

@racket[@#,hash-lang[] @#,racketmodname[s-exp] @#,racketmodname[envlang/tiny]]

@(table-of-contents)
@include-section[(submod "../test-tiny.hl.rkt" doc)]
@include-section[(submod "../demo-rkt.hl.rkt" doc)]
@include-section[(submod "../demo2-rkt.hl.rkt" doc)]
