#lang racket/base
(require (only-in racket/match match match-lambda match-let failure-cont ==)
         (only-in racket/list append-map)
         racket/set
         "static/object.rkt"
         "static/record.rkt"
         "static/sumtype.rkt"
         "monad.rkt"
         "disassemble.rkt"
         "parse.rkt"
         "vm.rkt"
         (prefix-in i: "instruction/opcode.rkt")
         "instruction/read.rkt"
         "instruction/version.rkt")

(define-sumtype Result
  (underway values ς ctx)
  (failure! message)
  (returned code ctx))

; get : key -> UPA a
(define-syntax get
  (syntax-rules ()
    [(_ key)
     (λ (ς ctx) (list (underway [values (list (hash-ref ς 'key))] ς ctx)))]
    [(_ key default)
     (λ (ς ctx) (list (underway [values (list (hash-ref ς 'key default))] ς ctx)))]))
; set : key a -> UPA ()
(define-syntax-rule (put key val)
  (λ (ς ctx) (list (underway [values (list)] [ς (hash-set ς 'key val)] ctx))))
; update : key (a a -> a) [a] -> UPA ()
(define-syntax update
  (syntax-rules ()
    [(_ key f)
     (λ (ς ctx) (list (underway [values (list)] [ς (hash-update ς 'key f)] ctx)))]
    [(_ key f iv)
     (λ (ς ctx) (list (underway [values (list)] [ς (hash-update ς 'key f iv)] ctx)))]))

(define-syntax p
  (syntax-rules ()
    [(_ unit who n)
     (λ xs (apply unit (build-list n (λ (i) (list* 'who i xs)))))]
    [(_ unit who)
     (p who 1)]))

(define concrete-stack%
  (inc (unit >>= >>
        panic)
       [push
        (λ (x) (update stack (λ (stk) (cons x stk)) (list)))]
       [pop
        (>>= (get stack (list))
             (match-lambda
               [(cons x stk)
                (>> (put stack stk)
                    (unit x))]
               [(list)
                (panic "tried to pop an empty stack")]))]))

(define concrete-cblock%
  (inc ()
       [get-intcblock
        (get intcblock)]
       [put-intcblock
        (λ (xs) (put intcblock xs))]
       [get-bytecblock
        (get bytecblock)]
       [put-bytecblock
        (λ (bss) (put bytecblock bss))]))

(define unit-assume/refute%
  (inc (unit)
       [assume (λ (c) (unit))]
       [refute (λ (c) (unit))]))

(define logic-assume/refute%
  (inc (unit >> mplus mzero)
       [! (p unit ! 1)]
       [&&
        (λ (x y)
          (if (and (exact-nonnegative-integer? x)
                   (exact-nonnegative-integer? y))
            (unit (if (or (zero? x)
                          (zero? y))
                    0 1))
            (unit `(&& 0 ,x ,y))))]
       [\|\|
        (λ (x y)
          (if (and (exact-nonnegative-integer? x)
                   (exact-nonnegative-integer? y))
            (unit (if (and (zero? x)
                           (zero? y))
                    0 1))
            (unit `(\|\| 0 ,x ,y))))]
       [assume
        (match-lambda
          [`(! 0 ,c)
           (refute c)]
          [`(&& 0 ,c₀ ,c₁)
           (>> (assume c₀)
               (assume c₁))]
          [`(\|\| 0 ,c₀ ,c₁)
           (mplus (assume c₀)
                  (assume c₁))]
          [c
           ((super assume) c)])]
       [refute
        (match-lambda
          [`(! 0 ,c) (assume c)]
          [`(&& 0 ,c₀ ,c₁)
           (mplus (refute c₀)
                  (refute c₁))]
          [`(\|\| 0 ,c₀ ,c₁)
           (>> (refute c₀)
               (refute c₁))]
          [c
           ((super refute) c)])]))

(define literal-assume/refute%
  (inc (unit >> mplus mzero)
       [assume
        (match-lambda
          [(? exact-nonnegative-integer? x)
           (if (zero? x) mzero (unit))]
          [`(== 0
                ,(? exact-nonnegative-integer? x₀)
                ,(? exact-nonnegative-integer? x₁))
           (if (= x₀ x₁) (unit) mzero)]
          [`(== 0
                ,(? bytes? bs₀)
                ,(? bytes? bs₁))
           (if (bytes=? bs₀ bs₁) (unit) mzero)]
          [`(== 0 ,c₀ ,c₁)
           (if (equal? c₀ c₁) (unit) (failure-cont))]
          [c
           ((super assume) c)])]
       [refute
        (match-lambda
          [(? exact-nonnegative-integer? x)
           (if (zero? x) (unit) mzero)]
          [`(== 0
                ,(? exact-nonnegative-integer? x₀)
                ,(? exact-nonnegative-integer? x₁))
           (if (= x₀ x₁) mzero (unit))]
          [`(== 0
                ,(? bytes? bs₀)
                ,(? bytes? bs₁))
           (if (bytes=? bs₀ bs₁) mzero (unit))]
          [`(== 0 ,c₀ ,c₁)
           (if (equal? c₀ c₁) mzero (failure-cont))]
          [c
           ((super refute) c)])]))

(define standard-in-mode%
  (inc (unit >>= panic)
       [in-mode
        (λ (target-mode info)
          (>>= (get mode #f)
               (match-lambda
                 [#f
                  (put mode target-mode)]
                 [mode
                  (if (eq? mode target-mode)
                    (unit)
                    (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))]))

(define (present? f t)
  (letrec ([present? (match-lambda
                       [(== f)
                        #t]
                       [`(,_ ,_ . ,ts)
                        (ormap present? ts)]
                       [_
                        #f])])
    (present? t)))

(define transaction-property
  (inc (unit >>= >>)
       [transaction-property-get
        (λ (key)
           (λ (ς ctx)
             (match-let ([(list txn glbl glbl-state) ctx])
               (list (underway [values (list (hash-ref txn key))] ς ctx)))))]
       [transaction-property-put
        (λ (key val)
          (λ (ς ctx)
            (match-let ([(list txn glbl glbl-state) ctx])
              (list (underway [values (list)] ς [ctx (list (hash-set txn key val)
                                                           glbl
                                                           glbl-state)])))))]))

(define txn:application-id%
  (inc (unit >>= >>
        mzero
        transaction-property-get transaction-property-put)
       [⊓
        (λ (key X)
          (if (eq? key 'application-id)
            (>>= (transaction-property-get 'application-id)
                 (match-lambda
                   [#f
                    (transaction-property-put 'application-id X)
                    #;
                    (match X
                      [`(≠ 0)
                       `(≠ 0)]
                      [`(= 0)]
                      [])
                    #;
                    (transaction-property-put 'application-id X)]
                   [`(≠ 0)
                    (match X
                      [`(≠ 0)
                       (unit)]
                      [`(= 0)
                       mzero])]
                   [`(= 0)
                    (match X
                      [`(= 0)
                       (unit)]
                      [`(≠ 0)
                       mzero])]))
            ((super ⊓) key X)))]
       [initialize-context
        (>> (transaction-property-put 'application-id #f)
            (super initialize-context))]
       [assume
        (λ (c)
          (>> (if (present? (i:ApplicationID) c)
                (match c
                  [`(== 0 ,(i:ApplicationID) 0)
                   (⊓ 'application-id `(= 0))]
                  [`(== 0 0 ,(i:ApplicationID))
                   (⊓ 'application-id `(= 0))])
                (unit))
              ((super assume) c)))]
       [refute
        (λ (c)
          (>> (if (present? (i:ApplicationID) c)
                (match c
                  [`(== 0 ,(i:ApplicationID) 0)
                   (⊓ 'application-id `(≠ 0))]
                  [`(== 0 0 ,(i:ApplicationID))
                   (⊓ 'application-id `(≠ 0))])
                (unit))
              ((super refute) c)))]))

(define txn:rekey-to%
  (inc (unit >>= >>
        mzero
        transaction-property-get transaction-property-put)
       [⊓
        (λ (key X)
          (if (eq? key 'rekey-to)
            (>>= (transaction-property-get 'rekey-to)
                 (match-lambda
                   [#f
                    (transaction-property-put 'rekey-to X)]
                   [`(= ,Y)
                    ; make no attempt to refine
                    (unit)]))
            ((super ⊓) key X)))]
       [initialize-context
        (>> (transaction-property-put 'rekey-to #f)
            (super initialize-context))]
       [assume
        (λ (c)
          (>> (if (present? (i:RekeyTo) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(i:RekeyTo) ,X)
                                  (>>= (transaction-property-get 'rekey-to)
                                       (match-lambda
                                         [#f
                                          (transaction-property-put 'rekey-to `(= ,X))]
                                         [`(= ,Y)
                                          ; make no attempt to see whether they *must* be different
                                          ; could be less conservative, such as if they are both concrete
                                          (unit)]))]
                                 [`(== 0 ,X ,(i:RekeyTo))
                                  (loop `(== 0 ,(i:RekeyTo) ,X))])])
                  (loop c))
                (unit))
              ((super assume) c)))]
       [refute
        (λ (c)
          (>> (if (present? (i:RekeyTo) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(i:RekeyTo) ,X)
                                  (>>= (transaction-property-get 'rekey-to)
                                       (match-lambda
                                         [#f
                                          ; don't remember what it's *not* equal to
                                          (unit)]
                                         [`(= ,Y)
                                          ; they are the same expression => they have the same value
                                          (if (equal? X Y)
                                            mzero
                                            (unit))]))]
                                 [`(== 0 ,X ,(i:RekeyTo))
                                  (loop `(== 0 ,(i:RekeyTo) ,X))])])
                  (loop c))
                (unit))
              ((super refute) c)))]))

(define txn:on-completion%
  (inc (unit >>= >>
        mzero
        transaction-property-get transaction-property-put)
       [⊓
        (λ (key X)
          (if (eq? key 'on-completion)
            (>>= (transaction-property-get 'on-completion) 
                 (λ (oc)
                   (let ([oc (set-intersect oc X)])
                     (if (set-empty? oc)
                       mzero
                       (transaction-property-put 'on-completion oc)))))
            ((super ⊓) key X)))]
       [initialize-context
        (>> (transaction-property-put 'on-completion (seteqv 0 1 2 4 5))
            (super initialize-context))]
       [assume
        (λ (c)
          (>> (if (present? (i:OnCompletion) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(? exact-nonnegative-integer? x) ,(i:OnCompletion))
                                  (loop `(== 0 ,(i:OnCompletion) ,x))]
                                 [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? x))
                                  (⊓ 'on-completion (seteqv x))])])
                  (loop c))
                (unit))
              ((super assume) c)))]
       [refute
        (λ (c)
          (>> (if (present? (i:OnCompletion) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(? exact-nonnegative-integer? x) ,(i:OnCompletion))
                                  (loop `(== 0 ,(i:OnCompletion) ,x))]
                                 [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? x))
                                  (⊓ 'on-completion (set-subtract (seteqv 0 1 2 4 5) (seteqv x)))])])
                  (loop c))
                (unit))
              ((super refute) c)))]))

(define trace%
  (inc (>>)
       [step
        (>> (λ (ς ctx)
              (pretty-print ς)
              (list (underway [values (list)] ς ctx)))
            (super step))]))

(define (make-vm lsv)
  (mix #;trace%
       (vm/version lsv)
       concrete-stack%
       concrete-cblock%
       literal-assume/refute%
       logic-assume/refute%
       txn:application-id%
       txn:rekey-to%
       txn:on-completion%
       (inc (unit)
            [initialize-context
             (unit)])
       transaction-property
       unit-assume/refute%
       standard-in-mode%
       (inc (unit >>= >> mplus mzero
              assume refute)
            [panic
             (λ (template . args)
               (λ (ς ctx) (list (failure! [message (apply format template args)]))))]
            [return
             (λ (code)
               (if0 code
                    (λ (ς ctx) (list (returned [code 0] ctx)))
                    (λ (ς ctx) (list (returned [code 1] ctx)))))]
            [check-final
             (>>= (get pc)
                  (match-lambda
                    [(list)
                     (>>= (get stack (list))
                          (match-lambda
                            [(list)   (panic "stack is empty at end of program")]
                            [(list x) (return x)]
                            [_        (panic "stack has more than one value at end of program")]))]
                    [_
                     (unit)]))]
            [constant
             unit]
            [if0
             (λ (x m₀ m₁)
               (mplus (>> (refute x) m₀)
                      (>> (assume x) m₁)))]
            [is-zero
             (λ (x) (if0 x (unit #t) (unit #f)))]
            [transaction unit]
            [group-transaction (p unit group-transaction 1)]
            [transaction-array (p unit transaction-array 1)]
            [group-transaction-array (p unit group-transaction-array 1)]
            [global unit]
            [btoi (p unit btoi 1)]
            [itob (p unit itob 1)]
            [concat (p unit concat 1)]
            [substring (p unit substring 1)]
            [len (p unit len 1)]
            [balance (p unit balance 1)]
            [min-balance (p unit min-balance 1)]
            [sha256 (p unit sha256 1)]
            [sha512-256 (p unit sha512-256 1)]
            [u== (p unit == 1)]
            [u<  (p unit < 1)]
            [u+ (p unit + 1)]
            [u- (p unit - 1)]
            [u* (p unit * 1)]
            [u/ (p unit / 1)]
            [u& (p unit & 1)]
            [u% (p unit % 1)]
            [app-opted-in (p unit app-opted-in 1)]
            [app-local-get
             (λ (acct key)
               (>>= (get app-local (list))
                    (letrec ([lookup (match-lambda
                                       [(list)
                                        (unit `(app-local-get 0 ,acct ,key))]
                                       [(cons `(put ,put-acct ,put-key ,val)
                                              al)
                                        (if0 `(&& 0 (== 0 ,acct ,put-acct)
                                                  (== 0 ,key  ,put-key))
                                             (lookup al)
                                             (unit val))])])
                      lookup)))]
            [app-local-get-ex
             (λ (acct app key)
               (>>= (get app-local (list))
                    (letrec ([lookup (match-lambda
                                       [(list)
                                        (unit `(app-local-get-ex 0 ,acct ,app ,key)
                                          `(app-local-get-ex 1 ,acct ,app ,key))]
                                       [(cons `(put ,put-acct ,put-key ,val)
                                              al)
                                        (if0 `(&& 0 (== 0 ,acct ,put-acct)
                                                  (== 0 ,key  ,put-key))
                                             (lookup al)
                                             (unit val 1))])])
                      lookup)))]
            [app-local-put
             (λ (acct key val)
               (update app-local (λ (al) (cons `(put ,acct ,key ,val) al)) (list)))]
            [app-local-del
             (p unit app-local-del 1)]
            [app-global-get
             (λ (key)
               (>>= (get app-global (list))
                    (letrec ([lookup (match-lambda
                                       [(list)
                                        (unit `(app-global ,key))]
                                       [(cons `(put ,put-key ,val)
                                              ag)
                                        (if0 `(== 0 ,key ,put-key)
                                             (lookup ag)
                                             (unit val))])])
                      lookup)))]
            [app-global-put
             (λ (key val)
               (update app-global (λ (ag) (cons `(put ,key ,val) ag)) (list)))]
            [app-global-del
             (p unit app-global-del 1)]
            [app-global-get-ex
             (λ (app key)
               (>>= (get app-global (list))
                    (letrec ([lookup (match-lambda
                                       [(list)
                                        (unit `(app-global ,app ,key)
                                          `(app-global-exists ,app ,key))]
                                       [(cons `(put ,put-key ,val)
                                              al)
                                        (if0 `(== 0 ,key ,put-key)
                                             (lookup al)
                                             (unit val 1))])])
                      lookup)))]
            [asset-holding-get (p unit asset-holding-get 2)]
            [asset-params-get (p unit asset-params-get 2)]
            [load
             (λ (i)
               (>>= (get scratch-space (hasheqv))
                    (λ (ss)
                      (cond
                        [(hash-ref ss i #f) => unit]
                        [else
                         (>> #;(log "Scratch space slot ~a uninitialized. The Go VM implementation produces 0." i)
                             (unit 0))]))))]
            [store (λ (i x) (update scratch-space (λ (ss) (hash-set ss i x)) (hasheqv)))])
       (inc ()
            [jump
             (λ (cfg) (put pc cfg))])
       (instruction-version/version lsv)
       (inc ()
            [instruction-name
             (λ (instr) "<instruction name>")])
       (inc (panic
             unit >>= >>)
            [read-instruction
             (>>= (get pc)
                  (match-lambda
                    [(list)
                     (panic "attempt to read instruction at end of stream")]
                    [(cons instr cfg)
                     (>> (put pc cfg)
                         (unit instr))]))])
       (inc (unit)
            [logic-sig-version
             (get logic-sig-version)])
       monad+-extras
       (inc ()
            [mplus
             (λ ms (λ (ς ctx) (apply append (map (λ (m) (m ς ctx)) ms))))])
       monad-extras
       (inc ()
            [unit (λ values (λ (ς ctx) (list (underway values ς ctx))))]
            [>>= (λ (m f)
                   (λ (ς ctx)
                     (append-map
                      (sumtype-case-lambda Result
                        [(underway [values xs] ς ctx)
                         ((apply f xs) ς ctx)]
                        #:otherwise list)
                      (m ς ctx))))])))

(require racket/pretty)

(define (analyze vm ς ctx)
  (let ([→ (vm 'step)])
    (match ((vm 'initialize-context) ς ctx)
      [(list (underway [values (list)] ς ctx))
       (letrec ([loop (λ (seen finals ς ctx)
                        (if (set-member? seen ς)
                          (values seen finals)
                          (for/fold ([seen (set-add seen ς)]
                                     [finals finals])
                                    ([r (in-list (→ ς ctx))])
                            (match r
                              [(underway [values (list)] ς ctx)
                               (loop seen finals ς ctx)]
                              [(failure! message)
                               (values seen finals)]
                              [(returned code ctx)
                               (values seen
                                       (match code
                                         [1 (set-add finals ctx)]
                                         [0 finals]))]))))])
         (let-values ([(seen finals) (loop (set) (set) ς ctx)])
           finals))])
    
    #;
    (let loop ([ς ς]
               [ctx ctx])
      (foldl
       (λ (r fs)
         (match r
           [(underway [values (list)] ς ctx)
            (loop ς ctx)]
           [(failure! message)
            fs]
           [(returned code ctx)
            (match code
              [1
               (set-add fs ctx)]
              [0
               fs])]))
       (set)
       (→ ς ctx)))))

(record execution-context (approval-program
                           clear-state-program
                           global-num-byte-slice
                           global-num-uint
                           local-num-byte-slice
                           local-num-uint
                           global-state))

(define (run ctx mapped-constants)
  (match ctx
    [(execution-context approval-program
                        clear-state-program
                        global-num-byte-slice
                        global-num-uint
                        local-num-byte-slice
                        local-num-uint
                        global-state)
     (match (disassemble approval-program) 
       [(cons lsv cfg)
        (if (<= lsv 3)
          (let ([ctxs (analyze (fix (make-vm lsv))
                               (hasheq 'logic-sig-version lsv
                                       'pc                cfg
                                       'stack             (list)
                                       'scratch-space     (hasheqv)
                                       'intcblock         (list)
                                       'bytecblock        (list)
                                       'mapped-constants  mapped-constants)
                               (list (hasheq)
                                     #f
                                     #f)
                               #;
                               (list (hash (i:Receiver)           #f
                                           (i:OnCompletion)       (seteqv 0 1 2 4 5)
                                           (i:ApprovalProgram)    approval-program
                                           (i:ClearStateProgram)  clear-state-program
                                           (i:RekeyTo)            `(???)
                                           (i:GroupIndex)         #f
                                           (i:GlobalNumUint)      global-num-uint
                                           (i:GlobalNumByteSlice) global-num-byte-slice
                                           (i:LocalNumUint)       local-num-uint
                                           (i:LocalNumByteSlice)  local-num-byte-slice)
                                     (hash (i:LogicSigVersion) (for/set ([i (in-range 1 lsv)]) i)
                                           (i:GroupSize)       (for/set ([i (in-range 16)]) (add1 i)))
                                     global-state))])
            (for/set ([ctx (in-set ctxs)])
              (match-let ([(list txn glbl glbl-state) ctx])
                txn
                #;
                (hash (i) (hash-ref txn 'on-completion)
                      (i:RekeyTo)      (hash-ref txn 'on-completion))))) 
          (error 'unconstrained-property-analysis "does not support LogicSigVersion = ~a > 3" lsv))]
       [#f
        (error 'unconstrained-property-analysis "unable to read initial logic signature version")])]))

(define (analyze/raw-binary program-type bs constants)
  42)

(require json
         net/base64)

(define (analyze/json-package bs constants)
  (match (with-handlers ([exn:fail:read? (λ (e)
                                           (displayln (exn-message e))
                                           (exit 255))])
           (read-json (open-input-bytes bs)))
    [(hash-table ('id id)
                 ('params (hash-table ('approval-program    approval-program)
                                      ('clear-state-program clear-state-program)
                                      ('creator             creator)
                                      ('global-state        global-entries)
                                      ('global-state-schema (hash-table ('num-byte-slice global-num-byte-slice)
                                                                        ('num-uint       global-num-uint)))
                                      ('local-state-schema  (hash-table ('num-byte-slice local-num-byte-slice)
                                                                        ('num-uint       local-num-uint))))))
     (run (execution-context [approval-program     (base64-decode (string->bytes/utf-8 approval-program))]
                             [clear-state-program  (base64-decode (string->bytes/utf-8 clear-state-program))]
                             global-num-byte-slice
                             global-num-uint
                             local-num-byte-slice
                             local-num-uint
                             [global-state         (for/hash ([entry (in-list global-entries)])
                                                     (match entry
                                                       [(hash-table ('key key) ('value value))
                                                        (values (base64-decode (string->bytes/utf-8 key))
                                                                (match (hash-ref value 'type)
                                                                  [1 (base64-decode (string->bytes/utf-8 (hash-ref value 'bytes)))]
                                                                  [2 (hash-ref value 'uint)]))]))])
          constants)]
    [json
     (displayln #<<MESSAGE
Input JSON did not match expected format.
(Was it produced by the Algorand API v2?)
MESSAGE
                )
     #;
     (exit 255)]))

(provide analyze/raw-binary
         analyze/json-package)

(define (analyze/assembly asm)
  (match (parse asm) 
    [(cons lsv cfg)
     (pretty-print cfg)
        (if (<= lsv 3)
          (let ([ctxs (time
                       (analyze (fix (make-vm lsv))
                                (hasheq 'logic-sig-version lsv
                                        'pc                cfg
                                        'stack             (list)
                                        'scratch-space     (hasheqv)
                                        'intcblock         (list)
                                        'bytecblock        (list))
                                (list (hasheq)
                                      #f
                                      #f)))])
            (for/set ([ctx (in-set ctxs)])
              (match-let ([(list txn glbl glbl-state) ctx])
                txn))) 
          (error 'unconstrained-property-analysis "does not support LogicSigVersion = ~a > 3" lsv))]
       [#f
        (error 'unconstrained-property-analysis "unable to parse assembly")]))

(provide analyze/assembly)

(module+ main
  (require json
           net/base64)

  (require racket/port)
  
  (match (current-command-line-arguments)
    [(vector filenames ...)
     (for-each
      (λ (filename)
        (displayln filename)
        (with-handlers (#;[exn:fail? (λ (e) (displayln (exn-message e)))]
                        )
          (pretty-print (analyze/json-package (call-with-input-file filename port->bytes) (hash)))))
      filenames)]))
