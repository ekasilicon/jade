#lang racket/base
(require racket/match
         racket/set
         racket/pretty
         net/base64
         "../src/static/object.rkt"
         "../src/instruction/read.rkt"
         "../src/instruction/name.rkt"
         "../src/instruction/version.rkt"
         "../src/monad.rkt"
         "../src/read-byte.rkt"
         "../src/prefix.rkt"
         "../src/vm.rkt")

; the base64 representation of the bytecode
(define postmodern-app-approval
  #"BiAJAAHAhD0CBAaN4dbYAoCS9AEQJgUIdHJlYXN1cnkHdmVyc2lvbghpbnRlcmVzdANldXIGcmV3YXJkMRkiEkAAHTEZIQQSQAABADIEIxIxADIJEjEAKGQSERBEQgFuNhoAgAVwcmljZRJAAP0yBCMSQACfMgQlEkAAAQAyBCUSMwAQIxIQMwAHMgoSEDMACIGAiXoPEDMBECEFEhAzARgyCBIQMwEAMwAAEhAzARsjEhA3ARoAFyQPEDcBGgAXgYCglKWNHQ4QRDMACCQJNQA0ACtkCyQKNQE3ARoAFzUCNAE0AoFkCoGXAQsPRDQANAKIASG0PXIINQQ1AzQDNAAiIogAyjMAADQCIQYiiAC/QgC6MgQjEjEAMgkSMQAoZBIREEQ2GgApEkAANTYaACcEEkAAIjYaACoSQAARNhoAKBJAAAEAKDYcAWdCAH8qNhoBF2dCAHYnBDYaARdnQgBsKTYaARdnQgBjMgQjEjEAgAZmZWVkZXJkEhAxGyUSEEQrNhoBF2cyCmAyCngJgYDaxAkNMQBgIQcMEEAAIzIKYDIKeAmBgLSJEw1BAB8oZDIKYDIKeAkkCSIiiAARQgAMMQAhByIiiAAFQv/RI0M1CDUHNQY1BbE0ByISQAAeIQSyEDQFshQ0BrISNAeyETQIIxJBAB00BbIVQgAWI7IQNAWyBzQGsgg0CCMSQQAENAWyCbOJNQo1CbEhBbIQIrIZIQiyNCEIsjWAhwQGIAgAAdrT2tECoI0GwIQ9ZAIEJgYGaXNzdWVkB2NyZWF0b3IDZXVyBWFzc2V0CXRpbWVzdGFtcAhpbnRlcmVzdDEYIhJAAN0xGYEFEkAAAQAkgAh0cmVhc3VyeWU1BTUEMgQhBhIzABAhBxIQMwAUMgkSEDMAEihkEhAzABErZBIQMwEQgQYSEDMBGDIIEhAzAQAzAAASEDMBGyISEEQyCmCIAQEPQABzMQApZBJAACyIASwlCDIKYA1AABUxAIgBHiIiiACZNAQiIiOIAJFCAIwxACIiI4gAhkIAgYgA3CUIMgpgDUAAKIgA0CUMQAAVNASIAMYiIogAZTEAIiIjiABdQgBYMQAiIiOIAFJCAE00BCIiI4gAR0IAQjEAKWQSREL/hCcEMgdnKTYaAGeAB2RlcG9zaXQ2GgEXZyg2GgIXZycFNhoDF2eAB3ZlcnNpb242GgQXZys2GgUXZyNDNQM1AjUBNQCxNAIiEkAAHiEHshA0ALIUNAGyEjQCshE0AyMSQQAdNACyFUIAFiOyEDQAsgc0AbIINAMjEkEABDQAsgmziSQqZTUJNQgoZCEGCoEDCyEECzQICokkKmU1CTUIKGQhBQonBWQLMgcnBGQJC4Hgj4YPCiEECzQICokkgAZyZXdhcmRlNQc1BiQqZTUJNQgoZCEFCiEFNAYICyEECzQIComyHoAEBoEBQ7IfMwAAsho0CRayGjQKFrIaKmQWshopZBayGiEGFrIas4k=")

(define board-app-approval
  #"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")

; a simple disassembler
(define (disassemble b64-encoded-bytecode)
  (match-let* ([(cons lsv bytecode)
                (read-prefix (base64-decode b64-encoded-bytecode))]
               [read-byte
                (inc ()
                     [unit
                       (λ xs (λ (i) (cons xs i)))]
                     [>>=
                      (λ (m f)
                        (λ (i)
                          (match (m i)
                            [(cons xs i)
                             ((apply f xs) i)]
                            [#f #f])))]
                     [read-byte
                      (λ (i)
                        (and (< i (bytes-length bytecode))
                             (cons (list (bytes-ref bytecode i)) (add1 i))))])]
               [read-instruction ((fix (mix (instruction-read/version lsv)
                                            read-byte-extras
                                            monad-extras
                                            read-byte))
                                  'read-instruction)])
    (values lsv
            (let loop ([pc 0]
                       [assembly (hasheqv)])
              (match (read-instruction pc)
                [(cons (list instr) succ-pc)
                 (loop succ-pc
                       (hash-set assembly pc (cons instr succ-pc)))]
                [#f
                 (hash-set assembly pc (list))])))))

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

(define (make→ lsv assembly)
  ((fix (mix (inc ()
                  #;
                  [execute
                   (λ (instr)
                     (displayln instr)
                     ((super 'execute) instr))])
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
                   (λ (code) (λ (ς) `(return ,code ,ς)))]
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
                   (>>= (get 'pc 0)
                        (λ (pc)
                          (match (hash-ref assembly pc)
                            [(cons instr succ-pc)
                             (>> ((put 'pc) succ-pc)
                                 (unit instr))])))]
                  [check-final
                   (>>= (get 'pc 0)
                        (λ (pc)
                          (match (hash-ref assembly pc)
                            ; not at the end of the instruction stream
                            [(cons _ _)
                             (unit)]
                            ; we won't need this for the program we're analyzing
                            ; but we would make sure the stack had a single uint64
                            ; element and use it to determine the return value
                            [(list)
                             (error 'analyzer "check-final not implemented")])))]
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
                           (panic "tried to pop an empty stack")]))])
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
                              (panic "integer constant index too great")))))]
                  [put-bytecblock
                   (put 'bytecblock)]
                  [lookup-bytecblock
                   (λ (i)
                     (>>= (get 'bytecblock (list))
                          (λ (ics)
                            (if (< i (length ics))
                              (unit (list-ref ics i))
                              (panic "byte constant index too great")))))])
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
                     [`#s(CloseRemaniderTo) 'bytes]
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
                     [`#s(Accounts) 'bytes] ; 28
                     ; ...
                     [`#s(GlobalNumUint) 'uint64] ; 52
                     [`#s(GlobalNumByteSlice) 'uint64]
                     ; ...
                     )]
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
                     [`#s(MinBalance) (unit 'uint64)]
                     [`#s(GroupSize) (unit 'uint64)]
                     [`#s(LatestTimestamp) (unit 'uint64)]
                     [`#s(CreatorAddress) (unit 'bytes)]
                     [`#s(CurrentApplicationID) (unit 'uint64)]
                     [`#s(CurrentApplicationAddress) (unit 'bytes)])]
                  [app-global-get
                   (λ (key)
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (unit `(app-global ,key))]
                       [else
                        (raise (list 'app-global-get key))])
                     #;
                     (if (eq? key 'bytes)
                       (>>= (get 'table (hasheqv)))
                       (mplus (unit 'uint64)
                              (unit 'bytes))
                       (raise (list 'app-global-get key))))]
                  [app-global-put
                   (λ (key val)
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (upd 'global-storage (list) (λ (gs) (cons (list key val) gs)))]
                       [else
                        (raise (list 'app-global-put key val))]))]
                  [app-param-field-type
                   (match-lambda
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
                     (cond
                       ; key is exact
                       [(bytes? key)
                        (mplus (unit `(app-global ,app-id ,key) 1)
                               (unit 0 0))]
                       [else
                        (raise (list 'app-global-get-ex app-id key))]))]
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
                  [is-zero
                   (λ (x)
                     (if (exact-nonnegative-integer? x)
                       (unit (zero? x))
                       (>> (>>= (get 'table (hash))
                               (λ (σ)
                                 (cond
                                   [(unify x 'uint64 σ)
                                    => (put 'table)]
                                   [else
                                    (raise (list 'is-zero x))])))
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
                                   (>>= (get 'table (hash))
                                        (λ (σ)
                                          (cond
                                            [(unify τᵢ τ σ)
                                             => (put 'table)]
                                            [else
                                             #;(panic "~a expected ~a at position ~a but got ~a" name τᵢ i τ)
                                             (raise (list 'op name τₒs τᵢs τs))])))))
                             (apply unit τₒs))
                         (error 'op "~a: lengths don't match; got ~a but expected ~a" name τs τᵢs))))]
                  [u==
                   (λ (x₀ x₁)
                     (>> (>>= (get 'table (hash))
                             (λ (σ)
                               (cond
                                 [(unify (type-of x₀) (type-of x₁) σ)
                                  => (put 'table)]
                                 [else
                                  (raise (list 'u== x₀ x₁))])))
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
                   (op 'divmodw '(uint64 uint64 uint64 uint64) 'uint64 'uint64 'uint64 'uint64)])
             (inc (unit op)
                  [btoi
                   (op 'btoi '(uint64) 'bytes)]
                  [itob
                   (op 'btoi '(bytes) 'uint64)]
                  [concat
                   (op 'concat '(bytes) 'bytes 'bytes)])
             (inc (unit >>= >>
                   get put upd)
                  [jump
                   (λ (offset) (upd 'pc 0 (λ (pc) (+ pc offset))))]
                  [callsub
                   (λ (offset)
                     (>>= (get 'pc 0)
                          (λ (pc)
                            (>>= (get 'callstack (list))
                                 (λ (cstk)
                                   (>> ((put 'callstack) (cons pc cstk))
                                       ((put 'pc) (+ pc offset))))))))]
                  [retsub
                   (>>= (get 'callstack (list))
                        (match-lambda
                          [(cons pc cstk)
                           (>> ((put 'callstack) cstk)
                               ((put 'pc) pc))]))])
             (inc (unit >>= >>
                   get put upd)
                  [inner-transaction-begin
                   ((put 'itxns) (cons (hasheq) (list)))]
                  [inner-transaction-field
                   (λ (f v)
                     (>>= (get 'itxns (list))
                          (match-lambda
                            [(cons itxn itxns)
                             ((put 'itxns) (cons (hash-set itxn f v) itxns))])))]
                  [inner-transaction-submit
                   (>>= (get 'itxns (list))
                        (λ (itxns)
                          (>> (upd 'itxns-submitted (list) (λ (itxnss) (cons itxns itxnss)))
                              ((put 'itxns) (list)))))]
                  [inner-transaction
                   (λ (f)
                     (>>= (get 'itxns-submitted (list))
                          (λ (itxnss)
                            (>>= (get 'itxns (list))
                                 (λ (itxns)
                                   (unit `(itxn ,(length itxnss) ,(length itxns) ,f)))))))])
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
                               (panic "application assumed to be in mode ~a but must also be in mode ~a for ~a" mode new-mode instr-name))])))])))
   'step))


(define (typecheck b64-bytecode)
  (let*-values ([(lsv assembly) (disassemble b64-bytecode)]
                [(→) (make→ lsv assembly)]
                [(seen σs)
                 (let analyze ([ς (hasheq)]
                               [seen (set)]
                               [σs (set)])
                   (if (set-member? seen ς)
                     (values seen σs)
                     (let loop ([r (→ ς)]
                                [seen (set-add seen ς)]
                                [σs σs])
                       (match r
                         [`(normal () ,ς)
                          (analyze ς seen σs)]
                         [`(panic ,msg ,ς)
                          #;(displayln msg)
                          (values seen σs)]
                         [`(return ,code ,ς)
                          (values seen
                                  (set-add σs (hash-ref ς 'table (hash))))]
                         [`(both ,r₀ ,r₁)
                          (let-values ([(seen σs) (loop r₀ seen σs)])
                            (loop r₁ seen σs))]
                         [`(none)
                          (values seen σs)]))))])
    (for/fold ([σ₀ (hash)])
              ([σ (in-set σs)])
      (for/fold ([σ₀ σ₀])
                ([x₀ (in-hash-keys σ)])
        (let ([x₁ (walk x₀ σ)])
          (cond
            [(unify x₀ x₁ σ₀)
             => values]
            [else
             (raise (list 'join σ₀ σ x₀))]))))))


(define (typecheck-program b64-encoded-bytecode)
  (let ([σ (time (typecheck b64-encoded-bytecode))])
    (displayln "program is type safe, assuming:")
    (for ([(name τ) (in-hash σ)])
      (printf "  ~a has ~a type\n"
              (match name
                [`(app-global ,key)
                 (format "the global at key ~s" (bytes->string/utf-8 key))]
                [`(app-global ,app-id ,key)
                 (format "the global of app with id ~a at key ~s" app-id (bytes->string/utf-8 key))]
                [`(gload ,i)
                 (format "each prior scratchspace at slot ~a" i)]
                )
              τ))))

(typecheck-program postmodern-app-approval)
(typecheck-program board-app-approval)




