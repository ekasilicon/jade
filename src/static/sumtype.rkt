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
  (provide sumtype-info sumtype-info?)
  (define (sumtype-extension id)
    (cond
      [(syntax-local-value id (λ () #f))
       => (λ (info)
            (cond
              [(record-info? info)
               (list id)]
              [(sumtype-info? info)
               (sumtype-extension* (sumtype-info-variants info))]
              [else
               (raise-syntax-error #f "unknown sumtype" id)]))]
      [else
       (raise-syntax-error #f "unknown sumtype" id)]))
  (define (sumtype-extension* ids) (apply append (map sumtype-extension ids))))

(define-syntax (define-sumtype stx)
  (define-syntax-class variant
    #:attributes [id declaration predicate]
    (pattern (recname:id field:id ...)
             #:attr id #'recname
             #:attr declaration #'(record recname (field ...))
             #:attr predicate #'(record-predicate recname))
    (pattern recname:id
             #:when (record-info? (syntax-local-value #'recname (λ () #f)))
             #:attr id #'recname
             #:attr declaration #'(void)
             #:attr predicate #'(record-predicate recname))
    (pattern sumname:id
             #:when (sumtype-info? (syntax-local-value #'sumname (λ () #f)))
             #:attr id #'sumname
             #:attr declaration #'(void)
             #:attr predicate #'(sumtype-predicate sumname)))
  (syntax-parse stx
    [(_ sumname:id decl:variant ...)
     (let loop ([variant-ids (syntax->list #'(decl.id ...))]
                [record-ids (list)])
       (match variant-ids
         [(list)
          record-ids]
         [(cons variant-id variant-ids)
          (define (check-duplicate!)
            (cond
              [(for/first ([record-id (in-list record-ids)]
                           #:when (free-identifier=? variant-id record-id))
                 record-id)
               => (λ (id) (raise-syntax-error #f (format "duplicate variant ~a" (syntax->datum id)) #'sumname))]
              [else
               (void)]))
          (cond
            [(syntax-local-value variant-id (λ () #f))
             => (match-lambda
                  [(sumtype-info variant-ids* _)
                   (loop variant-ids (loop variant-ids* record-ids))]
                  [(record-info _ _ _ _ _ _)
                   (check-duplicate!)
                   (loop variant-ids (cons variant-id record-ids))]
                  [_
                   ; previously bound but not as a record
                   (check-duplicate!)
                   (loop variant-ids (cons variant-id record-ids))])]
            [else
             ; a new record definition
             (check-duplicate!)
             (loop variant-ids (cons variant-id record-ids))])]))
     #'(begin
         decl.declaration ...
         (define (? x)
           (or (decl.predicate x) ...))
         (define-syntax sumname
           (sumtype-info (list #'decl.id ...) #'?)))]))

(define-syntax sumtype-predicate
  (syntax-parser
    [(_ sumname:id)
     (match (syntax-local-value #'sumname (λ () #f))
       [(sumtype-info _ ?)
        ?]
       [(record-info _ _ _ _ ? _)
        ?]
       [_
        (raise-syntax-error 'sumtype-predicate "not a declared sumtype" #'sumname)])]))

(define-syntax (sumtype-case stx)
  (define-syntax-class variant-clause
    #:attributes [id clause]
    (pattern [(recname:id field ...) body ...]
             #:when (record-info? (syntax-local-value #'recname (λ () #f)))
             #:attr id #'recname
             #:attr clause #'[(recname field ...) body ...])
    (pattern [(sumname:id name) body ...]
             #:when (sumtype-info? (syntax-local-value #'sumname (λ () #f)))
             #:attr id #'sumname
             #:attr clause #'[(sumname name) body ...]))
  ; make sure that each variant is in list of variants
  ; make sure that each variant shows up at most once
  ; so that the semantics is that order doesn't matter
  #;
  (define (variants-subtract variants₀ variants₁)
    (foldl (λ (variant variants)
             (if (member variant variants free-identifier=?)
               (remove variant variants free-identifier=?)
               (if (member variant variants₀ free-identifier=?)
                 (raise-syntax-error #f "duplicate variant" variant)
                 (raise-syntax-error #f "unnecessary variant" variant))))
           variants₀
           variants₁))
  #;
  (define (variants-subtract* variants₀ variants₁s-stx)
    (foldl
     (λ (variants₁-stx variants₀)
       (variants-subtract variants₀ (syntax->list variants₁-stx)))
     variants₀
     (syntax->list variants₁s-stx)))
  (syntax-parse stx
    [(_ type:id e vc:variant-clause ... (~optional (~seq #:otherwise f)))
     (cond
       [(syntax-local-value #'type (λ () #f))
        => (match-lambda
             [(sumtype-info variants ?)
              ; we want to keep as much structure of variants as possible
              ; if the id we're looking for comes from the list of variants, simply remove it
              ; if not, we need to search for it.
              ; we first scan the list of variants looking for it.
              ; if we don't find it, we descend into a sumtype and recur.
              ; since we always scan the list of variants looking for it,
              ; we will match sumtypes to sumtypes.
              ; if we find it, we flatten the entire subtree that we
              ; searched, less the matched term, in place of the root.
              ; thus our error reporting will be as concise as our respect
              ; for the type structure.
              ; if it is a sumtype, we may not find it because it is from a different
              ; partition of the same extension.
              ; in this case, we need to find each member recursively, destroying our tree
              ; in the process (to some degree, at least).
              ; we can then read off the predicate from the list of variants, as well as the error message.
              
              ; this process assumes that every subtree is unique, which define-sumtype should ensure.

              ; we can use the same kind of logic to allow difference matching: all of one tree less
              ; some subtrees
              (define (telescope ys xs)
                (match ys
                  [(list) xs]
                  [(cons y ys)
                   (telescope ys (cons y xs))]))
              (define (find-variant* find-ins to-find)
                (let loop ([variantsb (list)]
                           [variantsa find-ins])
                  (match variantsa
                    [(list)
                     #f]
                    [(cons variant-id variantsa)
                     (cond
                       [(find-variant variant-id to-find)
                        => (λ (variants) (telescope variantsb (append variants variantsa)))]
                       [else
                        (loop (cons variant-id variantsb) variantsa)])])))
              (define (find-variant find-in to-find)
                (if (free-identifier=? find-in to-find)
                  (list)
                  (cond
                    [(syntax-local-value find-in (λ () #f))
                     => (match-lambda
                          [(record-info _ _ _ _ _ _)
                           #f]
                          [(sumtype-info variants _)
                           (find-variant* variants to-find)])]
                    [else
                     (raise-syntax-error #f "should not be reachable" find-in)])))
              (let ([remaining-variants (for/fold ([variant-ids variants])
                                                  ([variant-id (in-list (syntax->list #'(vc.id ...)))])
                                          (cond
                                            [(find-variant* variant-ids variant-id)
                                             => values]
                                            [else
                                             ; not found by name, which occurs when a different partition of the sumtype is considered
                                             ; for now, raise an error, but can easily add support for it later
                                             (raise-syntax-error #f "using wrong partition" #'type)]))])
                #`(match e
                    vc.clause ...
                    #,(if (attribute f)
                        (with-syntax ([(variant-id ...) remaining-variants])
                          #'[(? (λ (x) (or ((sumtype-predicate variant-id) x) ...)) x) (f x)])
                        (if (null? remaining-variants)
                          #'[_ (failure-cont)]
                          (raise-syntax-error #f (format "no case~a for ~a"
                                                         (if (= (length remaining-variants) 1) "" "s")
                                                         (match (map syntax->datum remaining-variants)
                                                           [(list v) v]
                                                           [(list v₀ v₁) (format "~a and ~a" v₀ v₁)]
                                                           [vs (string-join (map symbol->string vs) ", " #:before-last ", and ")]))
                                              stx)))
                    [v
                     (error 'sumtype-case "~v not a variant of ~a" v 'type)]))])]
       [else
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
        (cond
          [(syntax-local-value #'sumname (λ () #f))
           => (match-lambda
                [(sumtype-info variants _)
                 (with-syntax ([(recname ...) variants])
                   (expand-export #'(combine-out sumname recname ...) modes))])]
          [else
           (raise-syntax-error 'sumtype-out "not a declared sumtype" #'sumname)])]))))

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
                  #:otherwise (λ (_) 20))
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
                 #:otherwise (λ (_) 42))))

  (define-sumtype P1 a b c)
  (define-sumtype P2 d e f)
  (define-sumtype Q1 a c e)
  (define-sumtype Q2 b d f)
  (define-sumtype Q2′ b d)

  (define-sumtype P P1 P2)

  #;
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

  #;
  (check-equal? (sumtype-case P (a)
                  [(Q1 _)
                   "a, c, or e"]
                  [(Q2′ _)
                   "b or d"]
                  #:otherwise
                  (λ (_) "something else"))
                "a, c, or e")

  (check-equal? ((record-predicate a) (a))
                #t)
  (check-equal? ((record-predicate b) (a))
                #f)
  (check-equal? ((sumtype-predicate P1) (a))
                #t)
  (check-equal? ((sumtype-predicate P2) (a))
                #f)
  (check-equal? ((sumtype-predicate P) (a))
                #t))

