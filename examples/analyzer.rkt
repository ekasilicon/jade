#lang racket/base
(require racket/match
         racket/set
         racket/pretty
         net/base64
         "disassemble.rkt"
         "../src/static/object.rkt"
         "../src/instruction/name.rkt"
         "../src/instruction/version.rkt"
         "../src/monad.rkt"
         "../src/vm.rkt")

; the base64 representation of the bytecode
(define postmodern-app-approval
  #"BiAJAAHAhD0CBAaN4dbYAoCS9AEQJgUIdHJlYXN1cnkHdmVyc2lvbghpbnRlcmVzdANldXIGcmV3YXJkMRkiEkAAHTEZIQQSQAABADIEIxIxADIJEjEAKGQSERBEQgFuNhoAgAVwcmljZRJAAP0yBCMSQACfMgQlEkAAAQAyBCUSMwAQIxIQMwAHMgoSEDMACIGAiXoPEDMBECEFEhAzARgyCBIQMwEAMwAAEhAzARsjEhA3ARoAFyQPEDcBGgAXgYCglKWNHQ4QRDMACCQJNQA0ACtkCyQKNQE3ARoAFzUCNAE0AoFkCoGXAQsPRDQANAKIASG0PXIINQQ1AzQDNAAiIogAyjMAADQCIQYiiAC/QgC6MgQjEjEAMgkSMQAoZBIREEQ2GgApEkAANTYaACcEEkAAIjYaACoSQAARNhoAKBJAAAEAKDYcAWdCAH8qNhoBF2dCAHYnBDYaARdnQgBsKTYaARdnQgBjMgQjEjEAgAZmZWVkZXJkEhAxGyUSEEQrNhoBF2cyCmAyCngJgYDaxAkNMQBgIQcMEEAAIzIKYDIKeAmBgLSJEw1BAB8oZDIKYDIKeAkkCSIiiAARQgAMMQAhByIiiAAFQv/RI0M1CDUHNQY1BbE0ByISQAAeIQSyEDQFshQ0BrISNAeyETQIIxJBAB00BbIVQgAWI7IQNAWyBzQGsgg0CCMSQQAENAWyCbOJNQo1CbEhBbIQIrIZIQiyNCEIsjWAhwQGIAgAAdrT2tECoI0GwIQ9ZAIEJgYGaXNzdWVkB2NyZWF0b3IDZXVyBWFzc2V0CXRpbWVzdGFtcAhpbnRlcmVzdDEYIhJAAN0xGYEFEkAAAQAkgAh0cmVhc3VyeWU1BTUEMgQhBhIzABAhBxIQMwAUMgkSEDMAEihkEhAzABErZBIQMwEQgQYSEDMBGDIIEhAzAQAzAAASEDMBGyISEEQyCmCIAQEPQABzMQApZBJAACyIASwlCDIKYA1AABUxAIgBHiIiiACZNAQiIiOIAJFCAIwxACIiI4gAhkIAgYgA3CUIMgpgDUAAKIgA0CUMQAAVNASIAMYiIogAZTEAIiIjiABdQgBYMQAiIiOIAFJCAE00BCIiI4gAR0IAQjEAKWQSREL/hCcEMgdnKTYaAGeAB2RlcG9zaXQ2GgEXZyg2GgIXZycFNhoDF2eAB3ZlcnNpb242GgQXZys2GgUXZyNDNQM1AjUBNQCxNAIiEkAAHiEHshA0ALIUNAGyEjQCshE0AyMSQQAdNACyFUIAFiOyEDQAsgc0AbIINAMjEkEABDQAsgmziSQqZTUJNQgoZCEGCoEDCyEECzQICokkKmU1CTUIKGQhBQonBWQLMgcnBGQJC4Hgj4YPCiEECzQICokkgAZyZXdhcmRlNQc1BiQqZTUJNQgoZCEFCiEFNAYICyEECzQIComyHoAEBoEBQ7IfMwAAsho0CRayGjQKFrIaKmQWshopZBayGiEGFrIas4k=")

(define board-app-approval
  #"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")

(define (make→ lsv assembly)
  ((fix (mix (vm/version lsv)
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
             (inc (unit)
                  [!
                   (λ (x) (unit `(! ,x)))]
                  [\|\|
                   (λ (x₀ x₁) (unit `(\|\| ,x₀ ,x₁)))]
                  [&&
                   (λ (x₀ x₁) (unit `(&& ,x₀ ,x₁)))])
             (inc (unit >>= mplus
                   get put upd)
                  [constant
                   unit]
                  [transaction
                   unit]
                  [transaction-array
                   (λ (f i) (unit `(idx ,f ,i)))]
                  [group-transaction
                   (λ (i f) (unit `(txn ,i ,f)))]
                  [group-transaction-array
                   (λ (i₀ f i₁) (unit `(txn-idx ,i₀ ,f ,i₁)))]
                  [global
                   unit]
                  [app-global-get
                   (λ (key) (unit `(app-global ,key)))]
                  [app-global-put
                   (λ (key val) (upd 'global-storage (list) (λ (gs) (cons (list key val) gs))))]
                  [app-params-get
                   (λ (field app-id)
                     (mplus (unit `(app-param ,app-id ,field) 1)
                            (unit 0 0)))]
                  [asset-params-get
                   (λ (field asset-id)
                     (mplus (unit `(asset-param ,asset-id ,field) 1)
                            (unit 0 0)))]
                  [app-global-get-ex
                   (λ (app-id key)
                     (mplus (unit `(app-global ,app-id ,key) 1)
                            (unit 0 0)))]
                  [balance
                   (λ (app-id) (unit `(balance ,app-id)))]
                  [min-balance
                   (λ (app-id) (unit `(min-balance ,app-id)))])
             (inc (unit >>=
                   get upd)
                  [store
                   (λ (i x) (upd 'scratchspace (hasheqv) (λ (ss) (hash-set ss i x))))]
                  [load
                   (λ (i) (>>= (get 'scratchspace (hasheqv)) (λ (ss) (unit (hash-ref ss i 0)))))]
                  [gload
                   (λ (group-index i)
                     (unit `(load ,group-index ,i)))])
             (inc (unit mplus)
                  [is-zero
                   (λ (x)
                     (if (exact-nonnegative-integer? x)
                       (unit (zero? x))
                       (mplus (unit #t)
                              (unit #f))))]
                  [u==
                   (λ (x₀ x₁) (unit `(== ,x₀ ,x₁)))]
                  [u<
                   (λ (x₀ x₁) (unit `(< ,x₀ ,x₁)))]
                  [u+
                   (λ (x₀ x₁) (unit `(+ ,x₀ ,x₁)))]
                  [u-
                   (λ (x₀ x₁) (unit `(- ,x₀ ,x₁)))]
                  [u*
                   (λ (x₀ x₁) (unit `(* ,x₀ ,x₁)))]
                  [u/
                   (λ (x₀ x₁) (unit `(/ ,x₀ ,x₁)))]
                  [usqrt
                   (λ (x) (unit `(sqrt ,x)))])
             (inc (unit)
                  [mulw
                   (λ (x₀ x₁)
                     (unit `(hi (mulw ,x₀ ,x₁))
                           `(lo (mulw ,x₀ ,x₁))))]
                  [divmodw
                   (λ (x₀h x₀l x₁h x₁l)
                     (unit `(hi (divw ,x₀h ,x₀l ,x₁h ,x₁l))
                           `(lo (divw ,x₀h ,x₀l ,x₁h ,x₁l))
                           `(hi (modw ,x₀h ,x₀l ,x₁h ,x₁l))
                           `(lo (modw ,x₀h ,x₀l ,x₁h ,x₁l))))])
             (inc (unit)
                  [btoi (λ (x) (unit `(btoi ,x)))]
                  [itob (λ (x) (unit `(itob ,x)))]
                  [concat (λ (x₀ x₁) (unit `(concat ,x₀ x₁)))])
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


(define (analyze b64-bytecode)
  (let*-values ([(lsv assembly) (disassemble b64-bytecode)]
                [(→) (make→ lsv assembly)]
                [(seen finals)
                 (let analyze ([ς (hasheq)]
                               [seen (set)]
                               [finals (set)])
                   (if (set-member? seen ς)
                     (values seen finals)
                     (let loop ([r (→ ς)]
                                [seen (set-add seen ς)]
                                [finals finals])
                       (match r
                         [`(normal () ,ς)
                          (analyze ς seen finals)]
                         [`(panic ,msg ,ς)
                          (values seen finals)]
                         [`(return ,code ,ς)
                          (values seen
                                  (if (= code 1)
                                    (set-add finals ς)
                                    finals))]
                         [`(both ,r₀ ,r₁)
                          (let-values ([(seen finals) (loop r₀ seen finals)])
                            (loop r₁ seen finals))]
                         [`(none)
                          (values seen finals)]))))])
    finals))


(define (analyze-program b64-encoded-bytecode)
  (define (hash-remove* ς ks)
    (foldl (λ (k ς) (hash-remove ς k)) ς ks))
  (let ([finals (time (analyze b64-encoded-bytecode))])
    (displayln (set-count finals))
    (pretty-print
     (for/set ([ς (in-set finals)])
       (hash-remove* ς '(intcblock bytecblock mode stack scratchspace callstack pc))))))

(analyze-program postmodern-app-approval)
(analyze-program board-app-approval)




