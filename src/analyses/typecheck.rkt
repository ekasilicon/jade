#lang racket/base
(require racket/match
         racket/set
         net/base64
         "../static/object.rkt"
         "../jade-error.rkt"
         "../report.rkt"
         "../execution-context.rkt"
         "../instruction/name.rkt"
         "../instruction/version.rkt"
         "../monad.rkt"
         "../vm.rkt"
         "../assembly/control.rkt")

(define (type-of x)
  (cond
    [(bytes? x)
     'bytes]
    [(exact-nonnegative-integer? x)
     'uint64]
    ; could fold into the `else`
    [(symbol? x)
     x]
    ; name of an entity
    [else
     x]))
(define (walk t σ)
  (cond
    [(symbol? t)
     t]
    [(hash-ref σ t #f)
     => (λ (t) (walk t σ))]
    [else
     t]))
(define (unify t₀ t₁ σ)
  (let ([t₀ (walk t₀ σ)]
        [t₁ (walk t₁ σ)])
    (cond
      [(equal? t₀ t₁)
       σ]
      [(and (symbol? t₀)
            (symbol? t₁))
       #f]
      [(symbol? t₀)
       (hash-set σ t₁ t₀)]
      [else
       (hash-set σ t₀ t₁)])))

(define (make→ lsv)
  ((fix (mix (inc ()
                  #;
                  [execute
                   (λ (instr)
                     (displayln instr)
                     ((super 'execute) instr))])
             vm-pseudo
             (vm/version lsv)
             (instruction-name/version lsv)
             (instruction-version/version lsv)
             monad+-extras
             monad-extras
             (inc ()
                  [unit
                   (λ xs (λ (ς) `(normal ,xs ,ς)))]
                  [>>=
                   (λ (m f)
                     (λ (ς)
                       (let loop ([r (m ς)])
                         (match  r
                           [`(normal ,xs ,ς)
                            ((apply f xs) ς)]
                           [`(type-error ,msg ,ς)
                            `(type-error ,msg ,ς)]
                           [`(panic ,msg ,ς)
                            `(panic ,msg ,ς)]
                           [`(return ,code ,ς)
                            `(return ,code ,ς)]
                           [`(both ,r₀ ,r₁)
                            `(both ,(loop r₀) ,(loop r₁))]
                           [`(none)
                            `(none)]))))]
                  [panic
                   (λ (template . args)
                     (λ (ς)
                       `(panic ,(apply format template args) ,ς)))]
                  [return
                   (λ (code)
                     (if (eq? (type-of code) 'uint64)
                       (λ (ς) `(return ,code ,ς))
                       (λ (ς) `(type-error (format "return with non-integer code ~a" code) ,ς))))]
                  [mplus
                   (λ ms (λ (ς) (foldr (λ (m M) `(both ,(m ς) ,M)) `(none) ms)))])
             (inc ()
                  [get
                   (λ (k v) (λ (ς) `(normal ,(list (hash-ref ς k v)) ,ς)))]
                  [put
                   (λ (k) (λ (v) (λ (ς) `(normal () ,(hash-set ς k v)))))]
                  [upd
                   (λ (k d f) (λ (ς) `(normal () ,(hash-update ς k f d))))])
             (inc (unit >>= >>
                   get put)
                  [read-instruction
                   (>>= (get 'pc (list))
                        (match-lambda
                          [(cons instr pc)
                           (>> ((put 'pc) pc)
                               (unit instr))]))]
                  [check-final
                   (>>= (get 'pc (list))
                        (match-lambda
                          ; not at the end of the instruction stream
                          [(cons _ _)
                           (unit)]
                          [(list)
                           (>>= (get 'stack (list))
                                (match-lambda
                                  [(cons x stk)
                                   (if (null? stk)
                                     (if (eq? (type-of x) 'uint64)
                                       (unit)
                                       (λ (ς) `(type-error ,(format "value ~a does not have type uint64" x) ,ς)))
                                     (λ (ς) `(type-error ,(format "stack does not have single element; ~a followed by ~a" x stk) ,ς)))]
                                  [(list)
                                   (λ (ς) `(type-error "program ended with an empty stack" ,ς))]))]))]
                  [logic-sig-version
                   (unit lsv)])
             (inc (unit >>= >> panic
                   get put upd)
                  [push
                   (λ (x) (upd 'stack (list) (λ (stk) (cons x stk))))]
                  [pop
                   (>>= (get 'stack (list))
                        (match-lambda
                          [(cons x stk)
                           (>> ((put 'stack) stk)
                               (unit x))]
                          [(list)
                           (λ (ς) `(type-error "pop of an empty stack" ,ς))]))])
             (inc (unit >>= panic
                   put get)
                  [put-intcblock
                   (put 'intcblock)]
                  [lookup-intcblock
                   (λ (i)
                     (>>= (get 'intcblock (list))
                          (λ (ics)
                            (if (< i (length ics))
                              (unit (list-ref ics i))
                              (λ (ς) `(type-error "integer constant index too great" ,ς))))))]
                  [put-bytecblock
                   (put 'bytecblock)]
                  [lookup-bytecblock
                   (λ (i)
                     (>>= (get 'bytecblock (list))
                          (λ (ics)
                            (if (< i (length ics))
                              (unit (list-ref ics i))
                              (λ (ς) `(type-error "byte constant index too great" ,ς))))))])
             (inc (unit op)
                  [!
                   (op '! '(uint64) 'uint64)]
                  [\|\|
                   (op '\|\| '(uint64) 'uint64 'uint64)]
                  [&&
                   (op '&& '(uint64) 'uint64 'uint64)])
             (inc (unit >>= mplus
                   get put upd)
                  [constant
                   unit]
                  [transaction-field-type
                   (match-lambda
                     [`#s(Sender) 'bytes]
                     [`#s(Fee) 'uint64]
                     [`#s(FirstValid) 'uint64]
                     [`#s(FirstValidTime) 'uint64]
                     [`#s(LastValid) 'uint64]
                     [`#s(Note) 'bytes]
                     [`#s(Lease) 'bytes]
                     [`#s(Receiver) 'bytes]
                     [`#s(Amount) 'uint64]
                     [`#s(CloseRemainderTo) 'bytes]
                     [`#s(VotePK) 'bytes]
                     [`#s(SelectionPK) 'bytes]
                     [`#s(VoteFirst) 'uint64]
                     [`#s(VoteLast) 'uint64]
                     [`#s(VoteKeyDilution) 'uint64]
                     [`#s(Type) 'bytes]
                     [`#s(TypeEnum) 'uint64]
                     [`#s(XferAsset) 'uint64]
                     [`#s(AssetAmount) 'uint64]
                     [`#s(AssetSender) 'bytes]
                     [`#s(AssetReceiver) 'bytes]
                     [`#s(AssetCloseTo) 'bytes]
                     [`#s(GroupIndex) 'uint64]
                     [`#s(TxID) 'bytes]
                     [`#s(ApplicationID) 'uint64]
                     [`#s(OnCompletion) 'uint64]
                     [`#s(ApplicationArgs) 'bytes]
                     [`#s(NumAppArgs) 'uint64]
                     [`#s(Accounts) 'bytes]
                     [`#s(NumAccounts) 'uint64]
                     [`#s(ApprovalProgram) 'bytes]
                     [`#s(ClearStateProgram) 'bytes]
                     [`#s(RekeyTo) 'bytes]
                     [`#s(ConfigAsset) 'uint64]
                     [`#s(ConfigAssetTotal) 'uint64]
                     [`#s(ConfigAssetDecimals) 'uint64]
                     [`#s(ConfigAssetDefaultFrozen) 'uint64]
                     [`#s(ConfigAssetUnitName) 'bytes]
                     [`#s(ConfigAssetName) 'bytes]
                     [`#s(ConfigAssetURL) 'bytes]
                     [`#s(ConfigAssetMetadataHash) 'bytes]
                     [`#s(ConfigAssetManager) 'bytes]
                     [`#s(ConfigAssetReserve) 'bytes]
                     [`#s(ConfigAssetFreeze) 'bytes]
                     [`#s(ConfigAssetClawback) 'bytes]
                     [`#s(FreezeAsset) 'uint64]
                     [`#s(FreezeAssetAccount) 'bytes]
                     [`#s(FreezeAssetFrozen) 'uint64]
                     [`#s(Assets) 'uint64]
                     [`#s(NumAssets) 'uint64]
                     [`#s(Applications) 'uint64]
                     [`#s(NumApplications) 'uint64]
                     [`#s(GlobalNumUint) 'uint64]
                     [`#s(GlobalNumByteSlice) 'uint64]
                     [`#s(LocalNumUint) 'uint64]
                     [`#s(LocalNumByteSlice) 'uint64]
                     [`#s(ExtraProgramPages) 'uint64]
                     [`#s(Nonparticipation) 'uint64]
                     [`#s(Logs) 'bytes]
                     [`#s(NumLogs) 'uint64]
                     [`#s(CreatedAssetID) 'uint64]
                     [`#s(CreatedApplicationID) 'uint64]
                     [`#s(LastLog) 'bytes]
                     [`#s(StateProofPK) 'bytes]
                     [`#s(NumApprovalProgramPages) 'uint64]
                     [`#s(NumClearStateProgramPages) 'uint64])]
                  [transaction
                   (λ (f) (unit (transaction-field-type f)))]
                  [transaction-array
                   (λ (f i) (unit (transaction-field-type f)))]
                  [group-transaction
                   (λ (i f) (unit (transaction-field-type f)))]
                  [group-transaction-array
                   (λ (i₀ f i₁) (unit (transaction-field-type f)))]
                  [global
                   (match-lambda
                     [`#s(MinTxnFee) (unit 'uint64)]
                     [`#s(MinBalance) (unit 'uint64)]
                     [`#s(MaxTxnLife) (unit 'uint64)]
                     [`#s(ZeroAddress) (unit 'bytes)]
                     [`#s(GroupSize) (unit 'uint64)]
                     [`#s(LogicSigVersion) (unit 'uint64)]
                     [`#s(Round) (unit 'uint64)]
                     [`#s(LatestTimestamp) (unit 'uint64)]
                     [`#s(CurrentApplicationID) (unit 'uint64)]
                     [`#s(CreatorAddress) (unit 'bytes)]
                     [`#s(CurrentApplicationAddress) (unit 'bytes)]
                     [`#s(GroupID) (unit 'bytes)]
                     [`#s(OpcodeBudget) (unit 'uint64)]
                     [`#s(CallerApplicationID) (unit 'uint64)]
                     [`#s(CallerApplicationAddress) (unit 'bytes)])]
                  [app-global-get
                   (λ (key)
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (unit `(app-global ,key))]
                       [else
                        (λ (ς) `(type-error ,(format "app-global-get of non-static key (~a) not supported" key) ,ς))]))]
                  [app-global-put
                   (λ (key val)
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (let ([τ₀ `(app-global ,key)]
                              [τ₁ (type-of val)])
                          (>>= (get 'table (hash))
                               (λ (σ)
                                 (cond
                                   [(unify τ₀ τ₁ σ)
                                    => (put 'table)]
                                   [else
                                    (λ (ς) `(type-error ,(format "~a cannot have type ~a" (human-name `(app-global ,key)) τ₁) ,ς))]))))]
                       [else
                        (λ (ς) `(type-error ,(format "app-global-put of non-static key (~a) with value ~a not supported" key val) ,ς))]))]
                  [app-local-get
                   (λ (acct key)
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (unit `(app-local ,key))]
                       [else
                        (λ (ς) `(type-error ,(format "app-local-get of non-static key (~a) not supported" key) ,ς))]))]
                  [app-local-put
                   (λ (acct key val)
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (let ([τ₀ `(app-local ,key)]
                              [τ₁ (type-of val)])
                          (>>= (get 'table (hash))
                               (λ (σ)
                                 (cond
                                   [(unify τ₀ τ₁ σ)
                                    => (put 'table)]
                                   [else
                                    (λ (ς) `(type-error ,(format "local at key ~s cannot have type ~a" (bytes->string/utf-8 key) τ₁) ,ς))]))))]
                       [else
                        (λ (ς) `(type-error ,(format "app-local-put of non-static key (~a) with value ~a not supported" key val) ,ς))]))]
                  [app-local-get-ex
                   (λ (acct app-id key)
                     ; a key must be exact and have the same type for all applications
                     (cond
                       [(bytes? key)
                        (mplus (unit `(app-local ,key) 1)
                               (unit 0 0))]
                       [else
                        (λ (ς) `(type-error ,(format "app-local-get-ex of non-static key (~a) not supported" key) ,ς))]))]
                  [asset-holding-field-type
                   (match-lambda
                     [`#s(AssetBalance) 'uint64]
                     [`#s(AssetFrozen) 'uint64])]
                  [asset-holding-get
                   (λ (acct asset-id f)
                     (unit (asset-holding-field-type f) 'uint64))]
                  [app-param-field-type
                   (match-lambda
                     [`#s(AppApprovalProgram) 'bytes]
                     [`#s(AppClearStateProgram) 'bytes]
                     [`#s(AppGlobalNumUint) 'uint64]
                     [`#s(AppGlobalNumByteSlice) 'uint64]
                     [`#s(AppLocalNumUint) 'uint64]
                     [`#s(AppLocalNumByteSlice) 'uint64]
                     [`#s(AppExtraProgramPages) 'uint64]
                     [`#s(AppCreator) 'bytes]
                     [`#s(AppAddress) 'bytes])]
                  [app-params-get
                   (λ (field app-id)
                     (mplus (unit (app-param-field-type field) 1)
                            (unit 0 0)))]
                  [asset-param-field-type
                   (match-lambda
                     [`#s(AssetUnitName) 'bytes])]
                  [asset-params-get
                   (λ (asset-id field)
                     (mplus (unit (asset-param-field-type field) 1)
                            (unit 0 0)))]
                  [app-global-get-ex
                   (λ (app-id key)
                     ; a key must be exact and have the same type for all applications
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (mplus (unit `(app-global ,key) 1)
                               (unit 0 0))]
                       [else
                        (jade-error 'typechecker "app-global-get-ex of app ~a and non-static key (~a) not supported" app-id key)]))]
                  [balance
                   (λ (app-id) (unit 'uint64))]
                  [min-balance
                   (λ (app-id) (unit 'uint64))])
             (inc (unit >>=
                   get upd)
                  [store
                   (λ (i x) (upd 'scratchspace (hasheqv) (λ (ss) (hash-set ss i x))))]
                  [load
                   (λ (i) (>>= (get 'scratchspace (hasheqv)) (λ (ss) (unit (hash-ref ss i 0)))))]
                  [gload
                   ; could work out a lattice that collapsed to ⊤
                   ; when the `group-index` is uint64
                   ; but we'll treat all `group-index`s the same
                   (λ (group-index i)
                     (unit `(gload ,i)))])
             (inc (unit >>= >> mplus panic
                   pop
                   get put)
                  [assert-has-type
                   (λ (who τ₀ τ₁)
                     (>>= (get 'table (hash))
                          (λ (σ)
                            (cond
                              [(unify τ₀ τ₁ σ)
                               => (put 'table)]
                              [else
                               (λ (ς) `(type-error ,(format "~a: types ~a and ~a expected to match" who τ₀ τ₁) ,ς))]))))]
                  [is-zero
                   (λ (x)
                     (if (exact-nonnegative-integer? x)
                       (unit (zero? x))
                       (>> (assert-has-type 'is-zero x 'uint64)
                           (mplus (unit #t)
                                  (unit #f)))))]
                  [op
                   (λ (name τₒs . τᵢs)
                     (λ τs
                       (if (= (length τᵢs)
                              (length τs))
                         (>> (for/fold ([m (unit)])
                                       ([τᵢ (in-list (reverse τᵢs))]
                                        [τ (in-list (map type-of τs))]
                                        [i (in-naturals)])
                               (>> m
                                   (assert-has-type
                                    name #;(list 'op name τₒs τᵢs τs)
                                    τᵢ τ)))
                             (apply unit τₒs))
                         (jade-error 'typechecker "op ~a: lengths don't match; got ~a but expected ~a" name τs τᵢs))))]
                  [u==
                   (λ (x₀ x₁)
                     (>> (>>= (get 'table (hash))
                             (λ (σ)
                               (cond
                                 [(unify (type-of x₀) (type-of x₁) σ)
                                  => (put 'table)]
                                 [else
                                  (λ (ς) `(type-error ,(format "==: types of ~a and ~a expected to match" x₀ x₁) ,ς))])))
                         (unit 'uint64)))]
                  [u<
                   (op '< '(uint64) 'uint64 'uint64)]
                  [u+
                   (op '+ '(uint64) 'uint64 'uint64)]
                  [u-
                   (op '- '(uint64) 'uint64 'uint64)]
                  [u*
                   (op '* '(uint64) 'uint64 'uint64)]
                  [u/
                   (op '/ '(uint64) 'uint64 'uint64)]
                  [usqrt
                   (op 'usqrt '(uint64) 'uint64)])
             (inc (unit op)
                  [mulw
                   (op 'mulw '(uint64 uint64) 'uint64 'uint64)]
                  [divmodw
                   (op 'divmodw '(uint64 uint64 uint64 uint64) 'uint64 'uint64 'uint64 'uint64)]
                  [exp
                   (op 'expw '(uint64) 'uint64 'uint64)]
                  [expw
                   (op 'expw '(uint64 uint64) 'uint64 'uint64)])
             (inc (op)
                  [sha256
                   (op 'sha256 '(bytes) 'bytes)])
             (inc (unit op)
                  [btoi
                   (op 'btoi '(uint64) 'bytes)]
                  [itob
                   (op 'btoi '(bytes) 'uint64)]
                  [concat
                   (op 'concat '(bytes) 'bytes 'bytes)]
                  [extract
                   (op 'extract '(bytes) 'uint64 'uint64 'bytes)])
             (inc (unit >>= >>
                   get put upd)
                  [jump
                   (put 'pc)]
                  [callsub
                   (λ (tgt)
                     (>>= (get 'pc (list))
                          (λ (pc)
                            (>>= (get 'callstack (list))
                                 (λ (cstk)
                                   (>> ((put 'callstack) (cons pc cstk))
                                       ((put 'pc) tgt)))))))]
                  [retsub
                   (>>= (get 'callstack (list))
                        (match-lambda
                          [(cons pc cstk)
                           (>> ((put 'callstack) cstk)
                               ((put 'pc) pc))]))])
             (inc (unit >>= >>
                   get put upd
                   transaction-field-type)
                  [inner-transaction-begin
                   (unit)]
                  [inner-transaction-field
                   (λ (f v)
                     (>>= (get 'table (hash))
                          (λ (σ)
                            (let ([τ₀ (transaction-field-type f)]
                                  [τ₁ (type-of v)])
                              (cond
                                [(unify τ₀ τ₁ σ)
                                 => (put 'table)]
                                [else
                                 (λ (ς) `(type-error "inner transaction field expected value of type ~a but got value of type ~a" τ₀ τ₁))])))))]
                  [inner-transaction-submit
                   (unit)]
                  [inner-transaction
                   (λ (f) (unit (transaction-field-type f)))])
             (inc (unit >>= panic
                   get put)
                  [in-mode
                   (λ (new-mode instr-name)
                     (>>= (get 'mode #f)
                          (match-lambda
                            [#f
                             ((put 'mode) new-mode)]
                            [mode
                             (if (equal? mode new-mode)
                               (unit)
                               (λ (ς) `(type-error ,(format "application assumed to be in mode ~a but must also be in mode ~a for ~a" mode new-mode instr-name) ,ς)))])))])))
   'step))

(define (typecheck lsv cfg)
  (let ([→ (make→ lsv)])
    (let-values ([(seen σs type-errors)
                  (let analyze ([ς (hasheq 'pc cfg)]
                                [seen (set)]
                                [σs (set)]
                                [tes (set)])
                    (if (set-member? seen ς)
                      (values seen σs tes)
                      (let loop ([r (→ ς)]
                                 [seen (set-add seen ς)]
                                 [σs σs]
                                 [tes tes])
                        (match r
                          [`(normal () ,ς)
                           (analyze ς seen σs tes)]
                          [`(type-error ,msg ,ς)
                           (values seen
                                   σs
                                   (set-add tes msg))]
                          [`(panic ,msg ,ς)
                           (values seen σs tes)]
                          [`(return ,code ,ς)
                           (values seen
                                   (set-add σs (hash-ref ς 'table (hash)))
                                   tes)]
                          [`(both ,r₀ ,r₁)
                           (let-values ([(seen σs tes) (loop r₀ seen σs tes)])
                             (loop r₁ seen σs tes))]
                          [`(none)
                           (values seen σs tes)]))))])
      (for/fold ([σ₀ (hash)]
                 [tes type-errors])
                ([σ (in-set σs)])
        (for/fold ([σ₀ σ₀]
                   [tes tes])
                  ([x₀ (in-hash-keys σ)])
          (let ([x₁ (walk x₀ σ)])
            (cond
              [(unify x₀ x₁ σ₀)
               => (λ (σ₀) (values σ₀ tes))]
              [else
               (values σ₀
                       (set-add tes (format "cannot reconcile ~a and ~a at the same type"
                                            (human-name x₀)
                                            (human-name x₁))))])))))))


(define human-name
  (match-lambda
    ['uint64
     "a uint64 value"]
    ['bytes
     "a bytes value"]
    [`(app-global ,key)
     (format "the global at key ~s" (bytes->string/utf-8 key))]
    [`(app-local ,key)
     (format "the local at key ~s" (bytes->string/utf-8 key))]
    [`(gload ,i)
     (format "each prior scratchspace at slot ~a" i)]))

(require racket/port)

(define (top-level-typecheck asm ctx)
  (let-values ([(σ tes) (match-let ([(cons lsv cfg) (control-flow-graph asm)])
                          (typecheck lsv cfg))])
    (with-output-to-string
      (λ ()
        (match (if (set-empty? tes)
                 (let-values ([(type-errors assumptions)
                               (let ([globals (match ctx
                                                [#f
                                                 (hash)]
                                                [(execution-context global-state)
                                                 global-state])])
                                 (for/fold ([tes (set)]
                                            [ass (set)])
                                           ([(name τ) (in-hash σ)])
                                   (match name
                                     [`(app-global ,key)
                                      (cond
                                        [(hash-ref globals key #f)
                                         => (λ (v)
                                              (cond
                                                [(eq? (type-of v) τ)
                                                 (values tes ass)]
                                                [(equal? v 0)
                                                 (values tes
                                                         (set-add ass
                                                                  (format "~a has ~a type"
                                                                          (human-name `(app-global ,key))
                                                                          τ)))]
                                                [else
                                                 (values (set-add tes
                                                                  (format "~a does not have ~a type"
                                                                          (human-name `(app-global ,key))
                                                                          τ))
                                                         ass)]))]
                                        [else
                                         (values tes
                                                 (set-add ass
                                                          (format "~a has ~a type"
                                                                  (human-name `(app-global ,key))       
                                                                  τ)))])]
                                     [name
                                      (values tes
                                              (set-add ass
                                                       (format "~a has ~a type"
                                                               (human-name name)
                                                               τ)))])))])
                   (if (set-empty? type-errors)
                     `(type-safe ,assumptions)
                     `(not-type-safe ,type-errors)))
                 `(not-type-safe ,tes))
          [`(type-safe ,assumptions)
           (if (set-empty? assumptions)
             (displayln (format #<<MSG
Typecheck result: ~a

The program is type-safe.
MSG
                             OK))
             (begin
               (displayln (format #<<MSG
Typecheck result: ~a

The program is type-safe, assuming:

MSG
                               OK))
               (for ([assumption (in-set assumptions)])
                 (display "  ")
                 (displayln assumption))))]
          [`(not-type-safe ,type-errors)
           (displayln (format #<<MSG
Typecheck result: ~a

The program does NOT typecheck:

MSG
                          ALERT))
           (for ([type-error (in-set type-errors)])
             (display "  ")
             (displayln type-error))
           (displayln #<<MSG

All operations encountering type mismatches were deemed type-safe.

The program MAY in fact be type-safe but ensure type safety in a way
too subtle for the typechecker to recognize. Depending on the reported
type errors, it may be worthwhile to refactor the code to use data in
an obviously type-safe way.
MSG
                  )])))))

(provide (rename-out [top-level-typecheck typecheck]))
