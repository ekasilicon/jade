#lang racket/base
(require racket/match
         "../version.rkt"
         "../static/record.rkt"
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "base.rkt"
         "uint.rkt"
         "bytes.rkt"
         (prefix-in i: "../instruction.rkt")
         (for-syntax racket/base
                     racket/match
                     syntax/parse))

(record label (ℓ))

(provide label)

(define label-identifier
  (lift (λ (c cs) (label [ℓ (string->symbol (apply string c cs))]))
        (∘ (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))
           (p* (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")))) ))

(provide label-identifier)

(define guarded-label
  (guard label-identifier
         "a label"))

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

(provide make-instruction-parser)

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
          #'(∨ p ...))]
       [_
        (raise-syntax-error #f "not a sumtype" #'typename)])]))

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

(provide instruction-parser/version)

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

