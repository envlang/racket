#lang racket

(provide make-racket-proc)

(define (make-racket-proc f env)
  (λ args (f env args)))