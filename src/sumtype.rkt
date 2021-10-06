#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     racket/string
                     syntax/parse
                     racket/provide-transform)
         racket/match
         "record.rkt")

(begin-for-syntax
  (struct sumtype-info (variants))
  (provide sumtype-info sumtype-info?))

(define-syntax (define-sumtype stx)
  (define-syntax-class variant
    #:attributes [name declaration]
    (pattern recname:id
             #:when (record-info? (syntax-local-value #'recname (λ () #f)))
             #:attr name #'recname
             #:attr declaration #'(void))
    (pattern (recname:id field:id ...) 
             #:attr name #'recname
             #:attr declaration #'(record recname (field ...))))
  (syntax-parse stx
    [(_ name:id decl:variant ...)
     (let loop ([names (syntax->list #'(decl.name ...))])
       (match names
         [(list)
          #'(begin
              decl.declaration ...
              (define-syntax name
                (sumtype-info (list #'decl.name ...))))]
         [(cons name names)
          (cond
            [(for/first ([name₀ (in-list names)]
                         #:when (free-identifier=? name name₀))
               name₀)
             => (λ (name) (raise-syntax-error #f "duplicate variant" name))]
            [else
             (loop names)])]))]))

(require (for-syntax racket/pretty))

(define-syntax (sumtype-case stx)
  (define-syntax-class variant-clause
    #:attributes [variants clause]
    (pattern [(recname:id field ...) body ...]
             #:when (record-info? (syntax-local-value #'recname (λ () #f)))
             #:attr variants #'(recname)
             #:attr clause #'[(recname field ...) body ...])
    (pattern [((recname:id) ...) body ...]
             #:attr variants #'(recname ...)
             #:attr clause #'[(or (recname) ...) body ...]))
  ; make sure that each variant is in list of variants
  ; make sure that each variant shows up at most once
  (define (variants-subtract variants₀ variants₁)
    (foldl (λ (variant variants)
             (if (member variant variants free-identifier=?)
               (remove variant variants free-identifier=?)
               (if (member variant variants₀ free-identifier=?)
                 (raise-syntax-error #f "duplicate variant" variant)
                 (raise-syntax-error #f "unknown variant" variant))))
           variants₀
           variants₁))
  (define (variants-subtract* variants₀ variants₁s-stx)
    (foldl
     (λ (variants₁-stx variants₀)
       (variants-subtract variants₀ (syntax->list variants₁-stx)))
     variants₀
     (syntax->list variants₁s-stx)))
  (syntax-parse stx
    #:literals (else)
    [(_ type:id e vc:variant-clause ...)
     (match (syntax-local-value #'type (λ () #f))
       [(sumtype-info variants)
        (let ([remaining-variants (variants-subtract* variants #'(vc.variants ...))])
          (if (null? remaining-variants)
            #'(match e
                vc.clause ...
                [v
                 (error 'sumtype-case "~v not a variant of ~a" v 'type)])
            (raise-syntax-error #f (format "no case~a for ~a"
                                           (if (= (length remaining-variants) 1) "" "s")
                                           (match (map syntax->datum remaining-variants)
                                             [(list v) v]
                                             [(list v₀ v₁) (format "~a and ~a" v₀ v₁)]
                                             [vs (string-join (map symbol->string vs) ", " #:before-last ", and ")]))
                                stx)))]
       [_
        (raise-syntax-error #f "not bound as sumtype" #'type)])]
    [(_ type:id e vc:variant-clause ... [else else-body ...])
     (match (syntax-local-value #'type (λ () #f))
       [(sumtype-info variants)
        (with-syntax ([(rest-variant ...) (variants-subtract* variants #'(vc.variants ...))])
          #'(match e
              vc.clause ...
              ; make sure that else clause checks the rest of variants
              [(or (rest-variant) ...)
               else-body ...]
              [v
               (error 'sumtype-case "~v not a variant of ~a" v 'type)]))]
       [_
        (raise-syntax-error #f "not bound as sumtype" #'type)])]))

; FIXME: report errors in terms of the expansion location
(define-syntax-rule (sumtype-case-lambda type . rst)
  (λ (x) (sumtype-case type x . rst)))

(define-syntax sumtype-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ sumname:id)
        (match (syntax-local-value #'sumname (λ () #f))
          [(sumtype-info variants)
           (with-syntax ([(recname ...) variants])
             (expand-export #'(combine-out sumname recname ...) modes))]
          [_
           (raise-syntax-error #f "not a sumtype binding" #'sumname)])]))))

(provide define-sumtype
         sumtype-case
         sumtype-case-lambda
         sumtype-out)

; 1. allow the else form of a sumtype-case-lambda to name the argument
; 2. make sumtype user-extensible? define-sumtype-clause?


