#lang racket/base
(require (only-in racket/match match match-lambda failure-cont)
         (only-in racket/string string-join)
         "../version.rkt"
         "../static/record.rkt"
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../instruction.rkt"
         (for-syntax racket/base
                     racket/match
                     syntax/parse))

(define-syntax enumtype-shower
  (syntax-parser
    [(_ enumtype-id:id method-id:id)
     (define (shower variant-id)
       (cond
         [(syntax-local-value variant-id (λ () #f))
          => (match-lambda
               [(sumtype-info _ _)
                (with-syntax ([variant-id variant-id])
                  #'[(variant-id x) ((super 'method-id) x)])]
               [(record-info _ (list) _ _ _ _)
                (with-syntax ([variant-id variant-id]
                              [variant-name (symbol->string (syntax->datum variant-id))])
                  #'[(variant-id) variant-name])])]
         [else
          (raise-syntax-error #f "not reachable" variant-id)]))
     (match (syntax-local-value #'enumtype-id (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(clause ...) (map shower variants)])
          #'(sumtype-case-lambda enumtype-id clause ...))]
       [_
        (raise-syntax-error #f "not a sumtype" #'enumtype-id)])]))

(define (show-instruction name . arguments)
  (string-join (cons name arguments) " "))

(define-syntax instruction-shower
  (syntax-parser
    [(_ instruction-id:id)
     (define (shower variant-id)
       (cond
         [(syntax-local-value variant-id (λ () #f))
          => (match-lambda
               [(sumtype-info _ _)
                (with-syntax ([variant-id variant-id])
                  #'[(variant-id instr) ((super 'instruction-show) instr)])]
               [(record-info _ fields _ cons _ _)
                (with-syntax ([variant-id variant-id]
                              [(field-id ...) fields]
                              [variant-name (symbol->string (syntax->datum variant-id))]
                              [(field-shower ...) (map
                                                   (match-lambda
                                                     ['v #'(self 'uint8-show)]
                                                     ['i #'(self 'uint8-show)]
                                                     ['n #'(self 'uint8-show)]
                                                     ['uint #'(self 'uint-show)]
                                                     ['uints #'(self 'uints-show)]
                                                     ['bytess #'(self 'bytess-show)]
                                                     ['group-index #'(self 'uint8-show)]
                                                     ['array-index #'(self 'uint8-show)]
                                                     ['offset #'(self 'label-show)]
                                                     ['start #'(self 'uint8-show)]
                                                     ['end #'(self 'uint8-show)]
                                                     ['length #'(self 'uint8-show)]
                                                     ['bytes #'(self 'bytes-show)]
                                                     ['field
                                                      (match (syntax->datum variant-id)
                                                        [(or 'txn 'gtxn 'txna 'gtxna 'gtxns 'gtxnsa 'itxn_field 'itxn 'itxna 'txnas 'gtxnas 'gtxnsas)
                                                         #'(self 'transaction-field-show)]
                                                        ['global #'(self 'global-field-show)]
                                                        ['asset_holding_get #'(self 'asset-holding-field-show)]
                                                        ['asset_params_get #'(self 'asset-params-field-show)]
                                                        ['app_params_get #'(self 'app-params-field-show)])])
                                            fields)])
                  #'[(variant-id field-id ...)
                     (show-instruction variant-name (field-shower field-id) ...)])])]
         [else
          (raise-syntax-error #f "not reachable" variant-id)]))
     (match (syntax-local-value #'instruction-id (λ () #f))
       [(sumtype-info variants _)
        (with-syntax ([(clause ...) (map shower variants)])
          #'(sumtype-case-lambda instruction-id clause ...))]
       [_
        (raise-syntax-error #f "not a sumtype" #'typename)])]))


(define instruction-show/version
  (make-*/version
   'instruction-show/version
   (inc ())
   (inc ()
        [instruction-show
         (instruction-shower Instruction1)]
        [transaction-field-show
         (enumtype-shower TransactionField1 transaction-field-show)]
        [global-field-show
         (enumtype-shower GlobalField1 global-field-show)])
   (inc ()
        [instruction-show
         (instruction-shower Instruction2)]
        [transaction-field-show
         (enumtype-shower TransactionField2 transaction-field-show)]
        [global-field-show
         (enumtype-shower GlobalField2 global-field-show)]
        [asset-holding-field-show
         (enumtype-shower AssetHoldingField2 asset-holding-field-show)]
        [asset-params-field-show
         (enumtype-shower AssetParamsField2 asset-params-field-show)])
   (inc ()
        [instruction-show
         (instruction-shower Instruction3)]
        [transaction-field-show
         (enumtype-shower TransactionField3 transaction-field-show)]
        [global-field-show
         (enumtype-shower GlobalField3 global-field-show)])
   (inc ()
        [instruction-show
         (instruction-shower Instruction4)]
        [transaction-field-show
         (enumtype-shower TransactionField4 transaction-field-show)])
   (inc ()
        [instruction-show
         (instruction-shower Instruction5)]
        [transaction-field-show
         (enumtype-shower TransactionField5 transaction-field-show)]
        [global-field-show
         (enumtype-shower GlobalField5 global-field-show)]
        [asset-params-field-show
         (enumtype-shower AssetParamsField5 asset-params-field-show)]
        [app-params-field-show
         (enumtype-shower AppParamsField5 app-params-field-show)])
   (inc ()
        [instruction-show
         (instruction-shower Instruction6)])
   (inc ())))

(provide instruction-show/version)

(module+ main
  (((fix (instruction-show/version 1))
    'instruction-show)
   (txn [field (Sender)])))
