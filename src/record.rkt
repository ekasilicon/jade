#lang racket/base
(require racket/match
         (for-syntax racket/base
                     racket/match
                     syntax/parse
                     syntax/parse/class/struct-id
                     #;
                     racket/struct-info))

(define-match-expander record
  (syntax-parser
    [(_ id:struct-id [field:id value] ...)
     (with-syntax ([x #'(id.accessor-id ...)])
       #''x)
     #;
     (match (extract-struct-info (syntax-local-value #'struct))
       [(list struct-id name-id )])])
  (syntax-parser
    [(_ id:struct-id [field:id value] ...)
     (with-syntax ([accessors #'(id.accessor-id ...)]
                   [suppliers #'(field ...)])
       #''(accessors suppliers))
     #;
     (match (extract-struct-info (syntax-local-value #'struct))
       [(list struct-id name-id )])])
  )




(module+ main
  (struct foo (bar baz) #:transparent)

(record foo [baz 2] [bar 1])

#;
  (match (record foo [bar 1] [baz 2])
    [(record foo [bar x] [baz y])
     (list x y)]))

