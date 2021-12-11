#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     racket/string
                     syntax/parse
                     racket/provide-transform)
         racket/match
         "record.rkt")

(begin-for-syntax
  (struct sumtype-info (variants ?)
    #:property prop:match-expander
    (λ (self stx)
      (match-let ([(sumtype-info _ ?) self])
        (with-syntax ([sumtype-predicate ?])
          (syntax-parse stx
            [(_ name)
             #'(? sumtype-predicate name)])))))
  (provide sumtype-info sumtype-info?))

(define-syntax (define-sumtype stx)
  (define-syntax-class variant
    #:attributes [names declaration predicate]
    (pattern (recname:id field:id ...)
             #:attr names #'(recname)
             #:attr declaration #'(record recname (field ...))
             #:attr predicate #'(record-predicate recname))
    (pattern recname:id
             #:when (record-info? (syntax-local-value #'recname (λ () #f)))
             #:attr names #'(recname)
             #:attr declaration #'(void)
             #:attr predicate #'(record-predicate recname))
    (pattern sumname:id
             #:when (sumtype-info? (syntax-local-value #'sumname (λ () #f)))
             #:with (recname ...) (sumtype-info-variants (syntax-local-value #'sumname))
             #:attr names #'(recname ...)
             #:attr declaration #'(void)
             #:attr predicate #'(sumtype-predicate sumname)))
  (syntax-parse stx
    [(_ sumname:id decl:variant ...)
     (let ([names (apply append (map syntax->list (syntax->list #'(decl.names ...))))])
       (let loop ([names names])
         (match names
           [(list)
            (void)]
           [(cons name names)
            (cond
              [(for/first ([name₀ (in-list names)]
                           #:when (free-identifier=? name name₀))
                 name₀)
               => (λ (name) (raise-syntax-error #f (format "duplicate variant ~a" (syntax->datum name)) #'sumname))]
              [else
               (loop names)])]))
       (with-syntax ([(decl-name ...) names])
         #'(begin
             decl.declaration ...
             (define (? x)
               (or (decl.predicate x) ...))
             (define-syntax sumname
               (sumtype-info (list #'decl-name ...) #'?)))))]))

(define-syntax sumtype-predicate
  (syntax-parser
    [(_ sumname:id)
     (match (syntax-local-value #'sumname (λ () #f))
       [(sumtype-info _ ?)
        ?]
       [(record-info _ _ _ ? _ _)
        ?]
       [_
        (raise-syntax-error 'sumtype-predicate "not a declared sumtype" #'sumname)])]))


(require (for-syntax racket/pretty))

(define-syntax (sumtype-case stx)
  (define-syntax-class variant-clause
    #:attributes [variants clause]
    (pattern [(recname:id field ...) body ...]
             #:when (record-info? (syntax-local-value #'recname (λ () #f)))
             #:attr variants #'(recname)
             #:attr clause #'[(recname field ...) body ...])
    (pattern [(sumname:id name) body ...]
             #:when (sumtype-info? (syntax-local-value #'sumname (λ () #f)))
             #:with (recname ...) (sumtype-info-variants (syntax-local-value #'sumname))
             #:attr variants #'(recname ...)
             #:attr clause #'[(sumname name) body ...]))
  ; make sure that each variant is in list of variants
  ; make sure that each variant shows up at most once
  ; so that the semantics is that order doesn't matter
  (define (variants-subtract variants₀ variants₁)
    (foldl (λ (variant variants)
             (if (member variant variants free-identifier=?)
               (remove variant variants free-identifier=?)
               (if (member variant variants₀ free-identifier=?)
                 (raise-syntax-error #f "duplicate variant" variant)
                 (raise-syntax-error #f "unnecessary variant" variant))))
           variants₀
           variants₁))
  (define (variants-subtract* variants₀ variants₁s-stx)
    (foldl
     (λ (variants₁-stx variants₀)
       (variants-subtract variants₀ (syntax->list variants₁-stx)))
     variants₀
     (syntax->list variants₁s-stx)))
  (syntax-parse stx
    [(_ type:id e vc:variant-clause ...)
     (cond
       [(syntax-local-value #'type (λ () #f))
        => (match-lambda
             [(sumtype-info variants ?)
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
                                      stx)))])]
       [else
        (raise-syntax-error 'sumtype-case "not a declared sumtype" #'type)])]
    [(_ type:id e vc:variant-clause ... #:otherwise x:id else-body ...)
     (match (syntax-local-value #'type (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(rest-variant ...) (variants-subtract* variants #'(vc.variants ...))])
          #'(match e
              vc.clause ...
              ; make sure that else clause checks the rest of variants
              [(and (or (rest-variant) ...) x)
               else-body ...]
              [v
               (error 'sumtype-case "~v not a variant of ~a" v 'type)]))]
       [_
        (raise-syntax-error 'sumtype-case "not a declared sumtype" #'type)])]))

(define-syntax (sumtype-case-lambda stx)
  (syntax-parse stx
    [(_ type . rst)
     #`(λ (x) #,(syntax/loc stx (sumtype-case type x . rst)))]))

(define-syntax sumtype-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ sumname:id)
        (match (syntax-local-value #'sumname (λ () #f))
          [(sumtype-info variants _)
           (with-syntax ([(recname ...) variants])
             (expand-export #'(combine-out sumname recname ...) modes))])]
       [_
        (raise-syntax-error 'sumtype-out "not a declared sumtype" #'sumname)]))))

(provide define-sumtype
         sumtype-predicate
         sumtype-case
         sumtype-case-lambda
         sumtype-out)

; 1. allow the else form of a sumtype-case-lambda to name the argument
; 2. make sumtype user-extensible? define-sumtype-clause?

(module+ main
  (require rackunit)
  
  (define-sumtype Test
    (a)
    (b)
    (c))

  (check-equal? (sumtype-case Test (a)
                  [(a) 10]
                  #:otherwise b 20)
                10)

  (define-sumtype Test2
    Test
    (d))

  (check-equal? (sumtype-case Test2 (d)
                  [(d) 20]
                  [(a) 12]
                  [(b) 10]
                  [(c) 9])
                20)

  (define-sumtype Test4
    (e)
    (f))
  
  (define-sumtype Test3
    Test
    Test4)

  (define-sumtype Test5
    a
    b
    d
    e)

  (check-equal? (sumtype-case Test5 (a)
                  [(a) 10]
                  [(b) 11]
                  [(d) 12]
                  [(e) 143])
                10)

  (check-exn #rx"not a variant"
             (λ ()
               (sumtype-case Test (e)
                 #:otherwise _ 42)))

  (define-sumtype P1 a b c)
  (define-sumtype P2 d e f)
  (define-sumtype Q1 a c e)
  (define-sumtype Q2 b d f)
  (define-sumtype Q2′ b d)

  (define-sumtype P P1 P2)
  
  (check-equal? (sumtype-case P (a)
                  [(Q1 q)
                   "a, c, or e"]
                  [(Q2 q)
                   "b, d, or f"])
                "a, c, or e")

  ; syntax error
  ; no case for f
  #;
  (check-equal? (sumtype-case P (a)
                  [(Q1 q)
                   "a, c, or e"]
                  [(Q2′ q)
                   "b or d"]))

  (check-equal? (sumtype-case P (a)
                  [(Q1 _)
                   "a, c, or e"]
                  [(Q2′ _)
                   "b or d"]
                  #:otherwise _
                  "something else")
                "a, c, or e")

  (check-equal? ((sumtype-predicate a) (a))
                #t)
  (check-equal? ((sumtype-predicate b) (a))
                #f)
  (check-equal? ((sumtype-predicate P1) (a))
                #t)
  (check-equal? ((sumtype-predicate P2) (a))
                #f)
  (check-equal? ((sumtype-predicate P) (a))
                #t))

