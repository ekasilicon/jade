#lang racket/base
(require racket/match
         "../version.rkt"
         "../static/record.rkt"
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "base.rkt"
         "number.rkt"
         "bytes.rkt"
         (prefix-in i: "../instruction.rkt")
         (for-syntax racket/base
                     racket/match
                     syntax/parse))


#|

(require )
|#
#;
(define-syntax instructionp
  (syntax-parser
    [(_ recname:id)
     (match (syntax-local-value #'recname (λ () #f))
       [(record-info _ fields _ constructor _ _)
        (let ([recname (syntax->datum #'recname)])
          (with-syntax ([name (symbol->string recname)]
                        [cons constructor]
                        [(fieldp ...) (map
                                       (match-lambda
                                         ['field
                                          (match recname)])
                                       fields)])
          #'(make-instructionp name cons fieldp ...)))
        ])]))

#;
(module+ test
  (parse-success (enumtype-parser i:TransactionField5)
                 "FreezeAsset"
                 (i:FreezeAsset))

  (parse-success (enumtype-parser i:GlobalField5)
                 "Round"
                 (i:Round))

  (parse-success (enumtype-parser i:GlobalField2)
                 "LatestTimestamp"
                 (i:LatestTimestamp)))

#;
(define-syntax instruction-parser
  (syntax-parser
    [(_ instruction:id)
     (match-let ([(sumtype-info variants _) (syntax-local-value #'instruction)])
       (with-syntax ([(parser ...)
                      (map
                       (λ (variant)
                         (match-let ([(record-info fields _ constructor _ _ _) (syntax-local-value variant)])
                           ))
                       variants)])
         #'(∨ parser ...)))]))

#;
(define transaction-field
  (enumtype-parser i:TransactionField))

#;
(define global-field
  (enumtype-parser i:GlobalField))


#;
(define asset-holding-field
  (enumtype-parser i:AssetHoldingField))

#;
(define asset-params-field
  (enumtype-parser i:AssetParamsField))

#;
(define app-params-field
  (enumtype-parser i:AppParamsField))


#|
(define-sumtype Pseudoinstruction
  i:Instruction
  (int uint)
  (byte bytes))
|#


#|
(define instruction
  (>> whitespace*
      (instruction-parser)))
|#

#;
(define instruction-parse1
  (instruction-parser i:Instruction1))

(record label (ℓ))

(define label-identifier
  (lift (λ (c cs) (label [ℓ (string->symbol (apply string c cs))]))
        (∘ (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))
           (p* (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")))) ))

(define guarded-label
  (guard label-identifier
         "a label"))
#|

(require (for-syntax racket/base
                     racket/match
                     syntax/parse)
         "../static/record.rkt"
         "../static/sumtype.rkt")

|#


(define-for-syntax lift-parser
  (let ([cache (list)])
    (λ (sumtype-id variants)
      (define (enumtype-parser* variant-ids)
        (let-values ([(parsers names)
                      (let loop ([variant-ids variant-ids])
                        (match variant-ids
                          [(list)
                           (values (list) (list))]
                          [(cons variant-id variant-ids)
                           (let-values ([(parser names₀)
                                         (cond
                                           [(syntax-local-value variant-id (λ () #f))
                                            => (match-lambda
                                                 [(record-info _ _ _ _ _ _)
                                                  (with-syntax ([variant variant-id]
                                                                [name (symbol->string (syntax->datum variant-id))])
                                                    (values #'(>> (literal name) (unit (variant)))
                                                            (list #'name)))]
                                                 [(sumtype-info variants _)
                                                  (lift-parser variant-id variants)])]
                                           [else
                                            (raise-syntax-error #f "not reachable" variant-id)])]
                                        [(parsers names₁)
                                         (loop variant-ids)])
                             (values (cons parser parsers)
                                     (append names₀ names₁)))]))])
          (with-syntax ([(p ...) parsers])
            (values #'(∨ p ...) names))))
      (cond
        [(assoc sumtype-id cache free-identifier=?)
         => (match-lambda
              [(list* _ parser-id names)
               (values parser-id names)])]
        [else
         (let-values ([(parser names) (enumtype-parser* variants)])
           (let ([parser-id (syntax-local-lift-expression parser)])
             (set! cache (cons (list* sumtype-id parser-id names) cache))
             (values parser-id names)))]))))



(define-syntax enumtype-parser
  (syntax-parser
    [(_ typename:id)
     (match (syntax-local-value #'typename (λ () #f))
       [(sumtype-info variants _)
        (define (enumtype-parser* variant-ids)
          (let-values ([(parsers names)
                        (let loop ([variant-ids variant-ids])
                          (match variant-ids
                            [(list)
                             (values (list) (list))]
                            [(cons variant-id variant-ids)
                             (let-values ([(parser names₀)
                                           (cond
                                             [(syntax-local-value variant-id (λ () #f))
                                              => (match-lambda
                                                   [(record-info _ _ _ _ _ _)
                                                    (with-syntax ([variant variant-id]
                                                                  [name (symbol->string (syntax->datum variant-id))])
                                                      (values #'(>> (literal name) (unit (variant)))
                                                              (list #'name)))]
                                                   [(sumtype-info _ _)
                                                    #'(super XXX)])]
                                             [else
                                              (raise-syntax-error #f "not reachable" variant-id)])]
                                          [(parsers names₁)
                                           (loop variant-ids)])
                               (values (cons parser parsers)
                                       (append names₀ names₁)))]))])
            (with-syntax ([(p ...) parsers])
              (values #'(∨ p ...) names))))
        (raise 'here)
        #;
        (let-values ([(parser names) (lift-parser #'typename variants)])
          (with-syntax ([p parser]
                        [(name ...) names])
            #'(guard p name ...)))]
       [_
        (raise-syntax-error #f "not a sumtype" #'typename)])]))

(define-for-syntax (enumtype-methods guarded-method-name method-name typename)
  (match (syntax-local-value typename (λ () #f))
    [(sumtype-info variant-ids _)
     (let-values ([(parsers names)
                   (let loop ([variant-ids variant-ids])
                     (match variant-ids
                       [(list)
                        (values (list) (list))]
                       [(cons variant-id variant-ids)
                        (let-values ([(parser names₀)
                                      (cond
                                        [(syntax-local-value variant-id (λ () #f))
                                         => (match-lambda
                                              [(record-info _ _ _ _ _ _)
                                               (with-syntax ([variant variant-id]
                                                             [name (symbol->string (syntax->datum variant-id))])
                                                 (values #'(>> (literal name) (unit (variant)))
                                                         (list #'name)))]
                                              [(sumtype-info variant-ids _)
                                               (let-values ([(_ names) (loop variant-ids)])
                                                 (values #`(super #,method-name)
                                                         names))])]
                                        [else
                                         (raise-syntax-error #f "not reachable" variant-id)])]
                                     [(parsers names₁)
                                      (loop variant-ids)])
                          (values (cons parser parsers)
                                  (append names₀ names₁)))]))])
       (with-syntax ([method-name method-name]
                     [guarded-method-name guarded-method-name]
                     [(parser ...) parsers]
                     [(name ...) names])
         (list #'[method-name
                  (∨ parser ...)]
               #'[guarded-method-name
                  (guard method-name name ...)])))]
    [_
     (raise-syntax-error #f "not a sumtype" typename)]))

(define (make-instruction-parser name constructor . argument-parsers)
  (>> (literal name)
      (>>= (let loop ([arg-parsers argument-parsers])
             (match arg-parsers
               [(list)
                (unit (list))]
               [(cons arg-parser arg-parsers)
                (>> whitespace+
                    (>>= arg-parser
                         (λ (x)
                           (>>= (loop arg-parsers)
                                (λ (xs) (unit (cons x xs)))))))]))
           (λ (xs) (unit (apply constructor xs))))))

(require (for-syntax racket/pretty))

(define-syntax instruction-parser
  (syntax-parser
    [(_
      instruction-id:id
      transaction-field:id
      global-field:id
      asset-params-field:id
      asset-holding-field:id
      app-params-field:id)
     (define (parser variant-id)
       (cond
         [(syntax-local-value variant-id (λ () #f))
          => (match-lambda
               [(sumtype-info _ _)
                #'(super instruction)]
               [(record-info _ fields _ cons _ _)
                (with-syntax ([name (symbol->string (syntax->datum variant-id))]
                              [constructor cons]
                              [(field ...) (map
                                            (match-lambda
                                              ['v #'guarded-uint8]
                                              ['i #'guarded-uint8]
                                              ['n #'guarded-uint8]
                                              ['uints #'(p* (>> whitespace* varuint))]
                                              ['bytess #'(p* (>> whitespace* bytes))]
                                              ['group-index #'guarded-uint8]
                                              ['array-index #'guarded-uint8]
                                              ['offset #'guarded-label]
                                              ['start #'guarded-uint8]
                                              ['end #'guarded-uint8]
                                              ['length #'guarded-uint8]
                                              ['bytes #'guarded-bytes]
                                              ['uint #'guarded-varuint]
                                              ['field
                                               (match (syntax->datum variant-id)
                                                 [(or 'txn 'gtxn 'txna 'gtxna 'gtxns 'gtxnsa 'itxn_field 'itxn 'itxna 'txnas 'gtxnas 'gtxnsas)
                                                  #'transaction-field]
                                                 ['global #'global-field]
                                                 ['asset_holding_get #'asset-holding-field]
                                                 ['asset_params_get #'asset-params-field]
                                                 ['app_params_get #'app-params-field])])
                                            fields)])
                  #'(make-instruction-parser name constructor field ...))])]
         [else
          (raise-syntax-error #f "not reachable" variant-id)]))
     (match (syntax-local-value #'instruction-id (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(p ...) (map parser variants)])
          #;(pretty-print (syntax->datum #'(∨ p ...)))
          #'(∨ p ...))
             #;
             (let ([parser-id (syntax-local-lift-expression
                               (with-syntax ([(p ...) (map parser variants)])
                                 #'(∨ p ...)))])
               (set! cache (cons (cons #'instruction parser-id) cache))
               parser-id)]
            [_
             (raise-syntax-error #f "not a sumtype" #'typename)])
     #;
     (cond
         #;
         [(assoc #'instruction cache free-identifier=?)
          => (match-lambda
               [(cons _ parser-id)
                parser-id])]
         [else
          ])]))

(define-syntax instruction-mixin
  (syntax-parser
    [(_ instruction-type:id
        (~optional (~seq #:transaction-field transaction-field-type:id))
        (~optional (~seq #:global-field global-field-type:id))
        (~optional (~seq #:asset-params-field asset-params-field-type:id))
        (~optional (~seq #:asset-holding-field asset-holding-field-type:id))
        (~optional (~seq #:app-params-field app-params-field-type:id)))
     (with-syntax ([(method ...)
                    (append (if (attribute transaction-field-type)
                              (enumtype-methods #'transaction-field
                                                #'transaction-field-raw
                                                #'transaction-field-type)
                              (list))
                            (if (attribute global-field-type)
                              (enumtype-methods #'global-field
                                                #'global-field-raw
                                                #'global-field-type)
                              (list))
                            (if (attribute asset-params-field-type)
                              (enumtype-methods #'asset-params-field
                                                #'asset-params-field-raw
                                                #'asset-params-field-type)
                              (list))
                            (if (attribute asset-holding-field-type)
                              (enumtype-methods #'asset-holding-field
                                                #'asset-holding-field-raw
                                                #'asset-holding-field-type)
                              (list))
                            (if (attribute app-params-field-type)
                              (enumtype-methods #'app-params-field
                                                #'app-params-field-raw
                                                #'app-params-field-type)
                              (list)))])
       #;
       (pretty-print
        (syntax->datum
         #'(inc (transaction-field
               global-field
               asset-params-field
               asset-holding-field
               app-params-field)
              [instruction
               (instruction-parser instruction-type
                                   transaction-field
                                   global-field
                                   asset-params-field
                                   asset-holding-field
                                   app-params-field)]
              method ...)))
       #'(inc (transaction-field
               global-field
               asset-params-field
               asset-holding-field
               app-params-field)
              [instruction
               (instruction-parser instruction-type
                                   transaction-field
                                   global-field
                                   asset-params-field
                                   asset-holding-field
                                   app-params-field)]
              method ...))]))


(define instruction-parser/version
  (let ([parser-object/version
         (make-*/version
          'instruction-parser/version
          (inc ()
               [transaction-field (λ xs (raise (cons 'transaction-field xs)))]
               [global-field (λ xs (raise (cons 'global-field xs)))])
          (instruction-mixin
           i:Instruction1
           #:transaction-field i:TransactionField1
           #:global-field i:GlobalField1)
          (instruction-mixin
           i:Instruction2
           #:transaction-field i:TransactionField2
           #:global-field i:GlobalField2
           #:asset-params-field i:AssetParamsField2
           #:asset-holding-field i:AssetHoldingField2)
          (instruction-mixin
           i:Instruction3
           #:transaction-field i:TransactionField3
           #:global-field i:GlobalField3)
          (instruction-mixin
           i:Instruction4
           #:transaction-field i:TransactionField4)
          (instruction-mixin
           i:Instruction5
           #:transaction-field i:TransactionField5
           #:global-field i:GlobalField5
           #:asset-params-field i:AssetParamsField5
           #:app-params-field i:AppParamsField5)
          (instruction-mixin
           i:Instruction6)
          (inc ()))])
    (λ (lsv) ((fix (parser-object/version lsv)) 'instruction))))

(module+ test
  (parse-success (instruction-parser/version 1)
                 "err"
                 (i:err))

  (parse-success (instruction-parser/version 1)
                 "txn Sender"
                 (i:txn [field (i:Sender)]))

  (parse-success (instruction-parser/version 2)
                 "addw"
                 (i:addw))

  (parse-success (instruction-parser/version 2)
                 "bnz END"
                 (i:bnz [offset (label [ℓ 'END])]))

  (parse-success (instruction-parser/version 2)
                 "substring 0 10"
                 (i:substring [start 0] [end 10]))

  (parse-success (instruction-parser/version 3)
                 "gtxnsa Fee 12"
                 (i:gtxnsa [field (i:Fee)] [array-index 12])))

