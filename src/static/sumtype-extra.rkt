#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     syntax/parse)
         racket/match
         "sumtype.rkt")

(define-syntax sumtype-name
  (syntax-parser
    [(_ typename:id)
     (match (syntax-local-value #'typename (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(variant ...)
                       variants]
                      [(variant-name ...)
                       (map symbol->string (map syntax->datum variants))])
          #'(sumtype-case-lambda typename
              [(variant) variant-name] ...))]
       [_
        (raise-syntax-error 'sumtype-name "not a declared sumtype" #'typename)])]))

#;
(define-syntax index→enumtype
  (syntax-parser
    [(_ typename:id)
     (match (syntax-local-value #'typename (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(i ...) (for/list ([i (in-naturals)]
                                          [_ (in-list variants)])
                                 i)]
                      [(variant ...) variants])
          #'(match-lambda [i (variant)] ...))]
       [_
        (raise-syntax-error 'index→enumtype "not a declared sumtype" #'typename)])]))

#;
(define-syntax enumtype-case
  (syntax-parser
    [(_ type:id expr [(name:id ...) body ...] ...)
     (with-syntax ([(sumname ...) (generate-temporaries #'((name ...) ...))])
       #'(let ()
           (define-sumtype sumname name ...)
           ...
           (sumtype-case type expr
             [(sumname _) body ...] ...)))]
    [(_ type:id expr [(name:id ...) body ...] ... #:otherwise x:id else-body ...)
     (with-syntax ([(sumname ...) (generate-temporaries #'((name ...) ...))])
       #'(let ()
           (define-sumtype sumname name ...)
           ...
           (sumtype-case type expr
             [(sumname _) body ...] ...
             #:otherwise x else-body ...)))]))

#;
(define-syntax (enumtype-case-lambda stx)
  (syntax-parse stx
    [(_ type . rst)
     #`(λ (x) #,(syntax/loc stx (enumtype-case type x . rst)))]))

(provide index→enumtype
         sumtype-name
         enumtype-case
         enumtype-case-lambda)
