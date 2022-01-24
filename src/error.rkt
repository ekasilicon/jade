#lang racket/base

(struct error-result (tag message))

(define (error tag template . args)
  (error-result tag (apply format template args)))

(provide error-result
         error)
