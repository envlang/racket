#lang info
(define collection "envlang")
(define deps '("base"
               "rackunit-lib"
               "phc-toolkit"))
(define build-deps '("base"
                     "reprovide-lang-lib"
                     "polysemy"))
(define scribblings '(("scribblings/envlang.scrbl" (multi-page))))
(define pkg-desc "A language with first-class-environments")
(define version "0.1")
(define pkg-authors '(|Suzanne Soy|))
