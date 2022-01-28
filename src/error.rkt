#lang racket/base
(require (only-in racket/match match))

(struct error-result (tag message))

(define (error tag template . args)
  (error-result tag (apply format template args)))

(define (error->>= x f)
  (match x
    [(error-result tag message)
     (error-result tag message)]
    [_
     (f x)]))

(provide error-result
         error
         error->>=)
