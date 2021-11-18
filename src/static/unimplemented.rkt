#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     syntax/parse)
         "record.rkt")

(define-syntax unimplemented
  (syntax-parser
    [(_ name:id init:field-init ...)
     #:when (record-info? (syntax-local-value #'name (λ () #f)))
     (match-let ([(record-info fields type constructor predicate accessor mutator)
                  (syntax-local-value #'name (λ () #f))])
       (with-syntax ([(field ...) (foldr remq fields (syntax->datum #'(init.field ...)))])
         #'(name init ...
                 [field (λ xs (error 'field "unimplemented; arguments are ~v" xs))] ...)))]))

(provide unimplemented)

(module+ main
  (require racket/match
           "monad.rkt")

  (match (unimplemented Monad
                        [unit (λ (x) x)])
    [(Monad unit >>=)
     (>>= 10)]))
