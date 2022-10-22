#lang racket/base
(require (only-in racket/match match match-lambda failure-cont)
         "../version.rkt"
         "../static/record.rkt"
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "base.rkt"
         "uint.rkt"
         "bytes.rkt"
         "../instruction.rkt"
         (for-syntax racket/base
                     racket/match
                     syntax/parse))


(define li-start
  (mc "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))

(define li-continuation
  (∨ li-start (mc "0123456789")))

(define label-identifier
  (fmap
   string->symbol
   (fmap
    list->string
    (fmap cons li-start (⋆p li-continuation)))))

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
                                                 (values #'(>> (litp name) (unit (variant)))
                                                         (list #'name)))]
                                              [(sumtype-info variant-ids _)
                                               (let-values ([(_ names) (loop variant-ids)])
                                                 (values #`(super '#,method-name)
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
  (>> (litp name)
      (>>= (let loop ([arg-parsers argument-parsers])
             (match arg-parsers
               [(list)
                (unit (list))]
               [(cons arg-parser arg-parsers)
                (>> space+
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
      app-params-field:id
      acct-params-field:id
      ecdsa-curve-field:id
      base64-encoding-field:id
      json-ref-type-field:id
      vrf-verify-standard-field:id
      block-field:id)
     (define (parser variant-id)
       (cond
         [(syntax-local-value variant-id (λ () #f))
          => (match-lambda
               [(sumtype-info _ _)
                #'(super 'instruction)]
               [(record-info _ fields _ cons _ _)
                (with-syntax ([name (symbol->string (syntax->datum variant-id))]
                              [constructor cons]
                              [(field ...) (map
                                            (match-lambda
                                              ['v #'guarded-uint8]
                                              ['i #'guarded-uint8]
                                              ['n #'guarded-uint8]
                                              ['s #'guarded-uint8]
                                              ['uints #'(delimitp space+ varuint)]
                                              ['bytess #'(delimitp space+ bytes)]
                                              ['index #'guarded-uint8]
                                              ['group-index #'guarded-uint8]
                                              ['array-index #'guarded-uint8]
                                              ['offset #'guarded-label]
                                              ['offsets #'(delimitp space+ label-identifier)]
                                              ['start #'guarded-uint8]
                                              ['end #'guarded-uint8]
                                              ['length #'guarded-uint8]
                                              ['bytes #'guarded-bytes]
                                              ['uint #'guarded-varuint]
                                              ['encoding #'base64-encoding-field]
                                              ['type #'json-ref-type-field]
                                              ['standard #'vrf-verify-standard-field]
                                              ['field
                                               (match (syntax->datum variant-id)
                                                 [(or 'txn 'gtxn 'txna 'gtxna 'gtxns 'gtxnsa 'itxn_field 'itxn 'itxna 'itxnas 'txnas 'gtxnas 'gtxnsas 'gitxn 'gitxna 'gitxnas)
                                                  #'transaction-field]
                                                 ['global #'global-field]
                                                 ['asset_holding_get #'asset-holding-field]
                                                 ['asset_params_get #'asset-params-field]
                                                 ['app_params_get #'app-params-field]
                                                 ['acct_params_get #'acct-params-field]
                                                 ['block #'block-field])])
                                            fields)])
                  #'(make-instruction-parser name constructor field ...))])]
         [else
          (raise-syntax-error #f "not reachable" variant-id)]))
     (match (syntax-local-value #'instruction-id (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(p ...) (map parser variants)])
          #'(∨ p ...))]
       [_
        (raise-syntax-error #f "not a sumtype" #'instruction-id)])]))

(define-syntax instruction-mixin
  (syntax-parser
    [(_ instruction-type:id
        (~optional (~seq #:transaction-field transaction-field-type:id))
        (~optional (~seq #:global-field global-field-type:id))
        (~optional (~seq #:asset-params-field asset-params-field-type:id))
        (~optional (~seq #:asset-holding-field asset-holding-field-type:id))
        (~optional (~seq #:app-params-field app-params-field-type:id))
        (~optional (~seq #:acct-params-field acct-params-field-type:id))
        (~optional (~seq #:ecdsa-curve-field ecdsa-curve-field-type:id))
        (~optional (~seq #:base64-encoding-field base64-encoding-field-type:id))
        (~optional (~seq #:json-ref-type-field json-ref-type-field-type:id))
        (~optional (~seq #:vrf-verify-standard-field vrf-verify-standard-field-type:id))
        (~optional (~seq #:block-field block-field-type:id)))
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
                              (list))
                            (if (attribute acct-params-field-type)
                              (enumtype-methods #'acct-params-field
                                                #'acct-params-field-raw
                                                #'acct-params-field-type)
                              (list))
                            (if (attribute ecdsa-curve-field-type)
                              (enumtype-methods #'ecdsa-curve-field
                                                #'ecdsa-curve-field-raw
                                                #'ecdsa-curve-field-type)
                              (list))
                            (if (attribute base64-encoding-field-type)
                              (enumtype-methods #'base64-encoding-field
                                                #'base64-encoding-field-raw
                                                #'base64-encoding-field-type)
                              (list))
                            (if (attribute json-ref-type-field-type)
                              (enumtype-methods #'json-ref-type-field
                                                #'json-ref-type-field-raw
                                                #'json-ref-type-field-type)
                              (list))
                            (if (attribute vrf-verify-standard-field-type)
                              (enumtype-methods #'vrf-verify-standard-field
                                                #'vrf-verify-standard-field-raw
                                                #'vrf-verify-standard-field-type)
                              (list))
                            (if (attribute block-field-type)
                              (enumtype-methods #'block-field
                                                #'block-field-raw
                                                #'block-field-type)
                              (list)))])
       #'(inc (transaction-field
               global-field
               asset-params-field
               asset-holding-field
               app-params-field
               acct-params-field
               ecdsa-curve-field
               base64-encoding-field
               json-ref-type-field
               vrf-verify-standard-field
               block-field)
              [instruction
               (instruction-parser instruction-type
                                   transaction-field
                                   global-field
                                   asset-params-field
                                   asset-holding-field
                                   app-params-field
                                   acct-params-field
                                   ecdsa-curve-field
                                   base64-encoding-field
                                   json-ref-type-field
                                   vrf-verify-standard-field
                                   block-field)]
              method ...))]))


(define instruction-parser/version
  (let ([parser-object/version
         (make-*/version
          'instruction-parser/version
          (inc ()
               [transaction-field (λ xs (raise (cons 'transaction-field xs)))]
               [global-field (λ xs (raise (cons 'global-field xs)))])
          (instruction-mixin
           Instruction1
           #:transaction-field TransactionField1
           #:global-field GlobalField1)
          (instruction-mixin
           Instruction2
           #:transaction-field TransactionField2
           #:global-field GlobalField2
           #:asset-params-field AssetParamsField2
           #:asset-holding-field AssetHoldingField2)
          (instruction-mixin
           Instruction3
           #:transaction-field TransactionField3
           #:global-field GlobalField3)
          (instruction-mixin
           Instruction4
           #:transaction-field TransactionField4)
          (instruction-mixin
           Instruction5
           #:transaction-field TransactionField5
           #:global-field GlobalField5
           #:asset-params-field AssetParamsField5
           #:app-params-field AppParamsField5)
          (instruction-mixin
           Instruction6
           #:transaction-field TransactionField6
           #:global-field GlobalField6
           #:acct-params-field AcctParamsField6)
          (instruction-mixin
           Instruction7
           #:ecdsa-curve-field ECDSACurve7
           #:base64-encoding-field Base64Encoding7
           #:json-ref-type-field JSONRefType7
           #:vrf-verify-standard-field VRFVerifyStandard7
           #:block-field BlockField7)
          (instruction-mixin
           Instruction8)
          (inc ()))])
    (λ (lsv) ((fix (parser-object/version lsv)) 'instruction))))

(provide instruction-parser/version)

(module+ test
  (parse-success (instruction-parser/version 1)
                 "err"
                 (err))

  (parse-success (instruction-parser/version 1)
                 "txn Sender"
                 (txn [field (Sender)]))

  (parse-success (instruction-parser/version 2)
                 "addw"
                 (addw))

  (parse-success (instruction-parser/version 2)
                 "bnz END"
                 (bnz [offset 'END]))

  (parse-success (instruction-parser/version 2)
                 "substring 0 10"
                 (substring [start 0] [end 10]))

  (parse-success (instruction-parser/version 3)
                 "gtxnsa Fee 12"
                 (gtxnsa [field (Fee)] [array-index 12]))

  (parse-success (instruction-parser/version 7)
                 "base64_decode URLEncoding"
                 (base64_decode [encoding (URLEncoding)]))

  (parse-success (instruction-parser/version 7)
                 "json_ref JSONString"
                 (json_ref [type (JSONString)]))

  (parse-success (instruction-parser/version 8)
                 "switch hello there"
                 (switch [offsets '(hello there)])))
