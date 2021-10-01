#lang racket/base
(require racket/match
         racket/set
         (only-in racket/list append-map)
         "monad.rkt"
         "read-byte.rkt"
         "prefix.rkt"
         "vm.rkt")

(struct underway (xs state))
(struct returned (code))
(struct failure! (msg))

; (define-type (Step a) (→ (State) (Result a))
(define step-Monad
  (Monad [unit (λ xs (λ (st) (list (underway xs st))))]
         [>>= (λ (m f)
                (λ (st)
                  (append-map
                   (match-lambda
                     [(underway xs st)
                      ((apply f xs) st)]
                     [r (list r)])
                   (m st))))]
         [>> (λ (m₀ m₁)
               (λ (st)
                 (append-map
                  (match-lambda
                    [(underway xs st)
                     (m₁ st)]
                    [r (list r)])
                  (m₀ st))))]))

(define ((return x) st)
  (list (returned x)))
(define ((panic template . args) st)
  (list (failure! (apply format template args))))


; get : key -> Step a
(define-syntax get
  (syntax-rules ()
    [(_ key)
     (λ (st) (list (underway (list (hash-ref st 'key)) st)))]
    [(_ key default)
     (λ (st) (list (underway (list (hash-ref st 'key default)) st)))]))
; set : key val -> Step ()
(define-syntax-rule (put key val)
  (λ (st) (list (underway (list) (hash-set st 'key val)))))
; update : key f -> Step ()
(define-syntax update
  (syntax-rules ()
    [(_ key f)
     (λ (st) (list (underway (list) (hash-update st 'key f))))]
    [(_ key f iv)
     (λ (st) (list (underway (list) (hash-update st 'key f iv))))]))

(define step-MonadPlus
  (MonadPlus [Monad step-Monad]
             [mzero (λ (st) (list))]
             [mplus (λ (m₀ m₁) (λ (st) (append (m₀ st) (m₁ st))))]))

(define step-ReadByte
  (match-let ([(Monad unit >>= >>) step-Monad])
    (ReadByte [Monad step-Monad]
              [read-byte (>>= (get bytecode)
                              (λ (bc)
                                (>>= (get pc)
                                     (λ (pc)
                                       (if (>= pc (bytes-length bc))
                                         (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                                         (>> (update pc add1)
                                             (unit (bytes-ref bc pc))))))))])))

(define-match-expander uint64
  (syntax-rules ()
    [(_ x)
     (? exact-nonnegative-integer? (? (λ (y) (< y (expt 2 64))) x))]))

(define-match-expander bytes
  (syntax-rules ()
    [(_ x)
     (? bytes? x)]))

; instance VM <hash-thing>
(define step-VM
  (match-let ([(MonadPlus [Monad (Monad unit >>= >>)] mzero mplus) step-MonadPlus])
    (define (interpretation #:assume assume #:reject reject)
      (match-lambda
        [`(¬ ,c) (reject c)]
        [c       (assume c)]))
    (define interpretations
      (list
       (interpretation
        #:assume
        (λ (c)
          (and (exact-nonnegative-integer? c)
               (if (zero? c) mzero (unit))))
        #:reject
        (λ (c)
          (and (exact-nonnegative-integer? c)
               (if (zero? c) (unit) mzero))))
       (interpretation
        #:assume
        (match-lambda
          [`(= ,x₀ ,x₁)
           (or (and (bytes? x₀)
                    (bytes? x₁)
                    (if (bytes=? x₀ x₁) (unit) mzero))
               (and (exact-nonnegative-integer? x₀)
                    (exact-nonnegative-integer? x₁)
                    (if (= x₀ x₁) (unit) mzero))
               (and (equal? x₀ x₁)
                    (unit)))]
          [_ #f])
        #:reject
        (match-lambda
          [`(= ,x₀ ,x₁)
           (or (and (bytes? x₀)
                    (bytes? x₁)
                    (if (bytes=? x₀ x₁) mzero (unit)))
               (and (exact-nonnegative-integer? x₀)
                    (exact-nonnegative-integer? x₁)
                    (if (= x₀ x₁) mzero (unit)))
               (and (equal? x₀ x₁)
                    mzero))]
          [_ #f]))
       (interpretation
        #:assume
        (letrec ([recur (match-lambda
                          [`(= OnCompletion ,oc)
                           (and (memq oc '(0 1 2 4 5))
                                (>>= (get OnCompletion #f)
                                     (match-lambda
                                       [#f
                                        (put OnCompletion `(known ,oc))]
                                       [`(known ,oc₀)
                                        (assume `(= ,oc₀ ,oc))]
                                       [`(unknown ,ocs)
                                        (for/fold ([m (put OnCompletion `(known, oc))])
                                                  ([oc₀ (in-list ocs)])
                                          (>> (reject `(= ,oc₀ ,oc))
                                              m))])))]
                          [`(= ,oc OnCompletion)
                           (recur `(= OnCompletion ,oc))]
                          [_ #f])])
          recur)
        #:reject
        (letrec ([recur (match-lambda
                          [`(= OnCompletion ,oc)
                           (and (memq oc '(0 1 2 4 5))
                                (>>= (get OnCompletion #f)
                                     (match-lambda
                                       [#f
                                        (put OnCompletion `(unknown ,(list oc)))]
                                       [`(known ,oc₀)
                                        (reject `(= ,oc₀ ,oc))]
                                       [`(unknown ,ocs)
                                        (if (memq oc ocs)
                                          (unit)
                                          (put OnCompletion `(unknown ,(cons oc ocs))))])))]
                          [`(= ,oc OnCompletion)
                           (recur `(= OnCompletion ,oc))]
                          [_ #f])])
          recur))
       
       (interpretation
        #:assume
        (match-lambda
          [`(< ,x ,x)
           mzero]
          [`(< ,(uint64 x) ,(uint64 y))
           (if (< x y)
             (unit)
             mzero)]
          [`(< ,(uint64 x) (exp 2 64))
           (if (< x (expt 2 64))
             (unit)
             mzero)]
          [_ #f])
        #:reject
        (match-lambda
          [`(< ,x ,x)
           (unit)]
          [`(< ,(uint64 x) ,(uint64 y))
           (if (< x y)
             mzero
             (unit))]
          [`(< ,(uint64 x) (exp 2 64))
           (if (< x (expt 2 64))
             mzero
             (unit))]
          [_ #f]))
       #;
       (interpretation
        #:assume
        (match-lambda
          [`(= (ApplicationArgs))]))))
    
    (define (push x) (update stack (λ (stk) (cons x stk))))
    (define pop
      (>>= (get stack)
           (match-lambda
             [(cons x stk)
              (>> (put stack stk)
                  (unit x))]
             [(list)
              (panic "tried to pop an empty stack")])))
    (define assume
      (match-lambda
        [`(¬ ,c)
         (reject c)]
        [`(∧ ,c₀ ,c₁)
         (>> (assume c₀)
             (assume c₁))]
        #;
        [`(∨ ,c₀ ,c₁)
         (mplus (assume c₀)
                (assume c₁))]
        [c
         (or (ormap (λ (i) (i c)) interpretations)
             (failure-cont))]
        [c
         (>>= (get path-condition)
              (λ (pc)
                (if (set-member? pc `(¬ ,c))
                  mzero
                  (update path-condition (λ (pc) (set-add pc c))))))]))
    (define reject
      (match-lambda
        [`(¬ ,c)
         (assume c)]
        [`(∨ ,c₀ ,c₁)
         (>> (reject c₀)
             (reject c₁))]
        #;
        [`(∧ ,c₀ ,c₁)
         (mplus (reject c₀)
                (reject c₁))]
        [c
         (or (ormap (λ (i) (i `(¬ ,c))) interpretations)
             (failure-cont))]
        [c
         (>>= (get path-condition)
              (λ (pc)
                (if (set-member? pc c)
                  mzero
                  (update path-condition (λ (pc) (set-add pc `(¬ ,c)))))))]))
    (define <rw
      (match-lambda
        [`(¬ ,c)
         `(¬ ,(<rw c))]
        [`(∧ ,c₀ ,c₁)
         `(∧ ,(<rw c₀)
             ,(<rw c₁))]
        [`(∨ ,c₀ ,c₁)
         `(∨ ,(<rw c₀)
             ,(<rw c₁))]
        [`(> ,x ,y)
         `(< ,y ,x)]
        [`(<= ,x ,y)
         (<rw `(>= ,y ,x))]
        [`(>= ,x ,y)
         `(¬ (< ,x ,y))]
        [c c]))
    (define (sif cnd thn els)
      (mplus (>> (assume cnd)
                 thn)
             (>> (reject cnd)
                 els)))
    (define (uint64-op2 s c x y)
      (match* (x y)
        [((uint64 x) (uint64 y))
         (unit (c x y))]
        [(_ _)
         (unit (list s x y))]))
    (define (log template . args)
      (update execution-log (λ (msgs) (cons (apply format template args) msgs))))
    (define (transaction-field f)
      (cond
        [(memq f '(Sender Fee FirstValid FirstValidTime LastValid Note Lease
                   Receiver Amount CloseRemainderTo VotePK SelectionPK VoteFirst
                   VoteLast VoteKeyDilution Type TypeEnum XferAsset AssetAmount
                   AssetSender AssetReceiver AssetCloseTo GroupIndex TxID))
         (unit f)]
        [(memq f '(ApplicationID OnCompletion ApplicationArgs NumAppArgs Accounts
                   NumAccounts ApprovalProgram ClearStateProgram RekeyTo ConfigAsset
                   ConfigAssetTotal ConfigAssetDecimals ConfigAssetDefaultFrozen
                   ConfigAssetUnitName ConfigAssetName ConfigAssetURL
                   ConfigAssetMetadataHash ConfigAssetManager ConfigAssetReserve
                   ConfigAssetFreeze ConfigAssetClawback FreezeAsset
                   FreezeAssetAccount FreezeAssetFrozen))
         (>> ((logic-sig-version>= step-VM) 2 (symbol->string f))
             (unit f))]
        [(memq f '(Assets NumAssets Applications NumApplications GlobalNumUint
                   GlobalNumByteSlice LocalNumUint LocalNumByteSlice))
         (>> ((logic-sig-version>= step-VM) 3 (symbol->string f))
             (unit f))]
        [(memq f '(ExtraProgramPages))
         (>> ((logic-sig-version>= step-VM) 4 (symbol->string f))
             (unit f))]
        [(memq f '(Nonparticipation Logs NumLogs CreatedAssetID
                   CreatedApplicationID))
         (>> ((logic-sig-version>= step-VM) 5 (symbol->string f))
             (unit f))]
        [else
         (error 'transaction-field "unknown transaction field ~a" f)]))
    (define (group-transaction gi f)
      (>>= (transaction-field f)
           (λ (f)
             (if (eq? gi 'this-group-index)
               (unit f)
               (unit `(txn ,gi ,f))))))
    (define (group-transaction-array gi f ai)
      (>>= (transaction-field f)
           (λ (f)
             (if (eq? gi 'this-group-index)
               (unit `(,f ,ai))
               (unit `(txn ,gi ,f ,ai))))))
    (VM [MonadPlus step-MonadPlus]
        [ReadByte step-ReadByte]
        panic
        [return! return]
        [logic-sig-version (get logic-sig-version)]
        ; could (assume `(= ApplicationMode ,target-mode)) and work the metalogic out with a custom interpretation
        [in-mode (λ (target-mode info)
                   (>>= (get mode #f)
                        (match-lambda
                          [#f
                           (put mode target-mode)]
                          [mode
                           (if (eq? mode target-mode)
                             (unit)
                             (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))] 
        [get-pc (get pc)]
        [set-pc (λ (pc) (put pc pc))]
        [get-bytecode (get bytecode)]
        [get-intcblock (get intcblock)]
        [put-intcblock (λ (xs) (put intcblock xs))]
        [get-bytecblock (get bytecblock)]
        [put-bytecblock (λ (bss) (put bytecblock bss))]
        push
        pop
        [push-call (λ (ret-pc) (update call-stack (λ (pcs) (cons ret-pc pcs))))]
        [pop-call (>>= (get call-stack)
                       (match-lambda
                         [(cons pc pcs)
                          (>> (put call-stack pcs)
                              (unit pc))]))]
        [sha256 (λ (x) (unit `(sha256 ,x)))]
        [+
         (λ (x y)
           (>>= (uint64-op2 '+ + x y)
                (λ (z)
                  (>> (log "using symbolic constant (exp 2 64) for +")
                      (sif (<rw `(< ,z (exp 2 64)))
                           (unit z)
                           (panic "~a + ~a overflows" x y))))))]
        [-
         (λ (x y)
           (sif (<rw `(<= ,x ,y))
                (uint64-op2 '- - x y)
                (panic "-: ~a > ~a" y x)))]
        [/
         (λ (x y)
           (sif y
                (unit `(/ ,x ,y))
                (panic "/: ~a is zero" y)))]
        [*
         (λ (x y)
           (>>= (uint64-op2 '* * x y)
                (λ (z)
                  (>> (log "using symbolic constant (exp 2 64) for *")
                      (sif (<rw `(< ,z (exp 2 64)))
                           (unit z)
                           (panic "~a * ~a overflows" x y))))))]
        [len (λ (x) (unit `(len ,x)))]
        [itob (λ (x) (unit `(itob ,x)))]
        [btoi
         (λ (x)
           (sif (<rw `(<= (len ,x) 8))
                (unit `(btoi ,x))
                (panic "btoi: input greater than 8 bytes")))]
        [%
         (λ (x y)
           (sif y
                (unit `(% ,x ,y))
                (panic "%: ~a is zero" y)))]
        [mulw
         (λ (a b)
           (cond
             [(and (exact-nonnegative-integer? a)
                   (exact-nonnegative-integer? b))
              (let ([c (* a b)])
                (>> (push (arithmetic-shift c -64))
                    (push (bitwise-and c (sub1 (expt 2 64))))))]
             [else
              (>> (push `(hi (mulw ,a ,b)))
                  (push `(lo (mulw ,a ,b))))]))]
        ; XXX handle literals
        [addw
         (λ (a b)
           (>> (push `(hi (addw ,a ,b)))
               (push `(lo (addw ,a ,b)))))]
        [divmodw
         (λ (a b c d)
           (let ([dend (match* (a b)
                         [((? exact-nonnegative-integer?) (? exact-nonnegative-integer?))
                          (bitwise-ior (* a (expt 2 64)) b)]
                         [(`(hi ,x) `(lo ,x)) x]
                         [(x y) `(uint128 ,x ,y)])]
                 [isor (match* (c d)
                         [((? exact-nonnegative-integer?) (? exact-nonnegative-integer?))
                          (bitwise-ior (* c (expt 2 64)) d)]
                         [(`(hi ,x) `(lo ,x)) x]
                         [(x y) `(uint128 ,x ,y)])])
             (sif isor
                  (>> (push `(hi (quotient ,dend ,isor)))
                      (>> (push `(lo (quotient ,dend ,isor)))
                          (>> (push `(hi (remainder ,dend ,isor)))
                              (push `(lo (remainder ,dend ,isor))))))
                  (panic "divmodw by 0"))))]
        [is-zero (λ (c) (sif c (unit #f) (unit #t)))]
        [&& (λ (x y) (unit `(∧ ,x ,y)))]
        [\|\| (λ (x y) (unit `(∨ ,x ,y)))]
        [= (λ (x y) (unit `(= ,x ,y)))]
        [< (λ (x y) (unit `(< ,x ,y)))]
        [!
         (match-lambda
           [`(¬ ,c) (unit c)]
           [c (unit `(¬ ,c))])]
        [~ (λ (x) (unit `(~ ,x)))]
        [concat (λ (x y) (unit `(concat ,x ,y)))]
        [substring
         (λ (a s e)
           (sif (<rw `(∧ (<= ,s ,e)
                         (<= ,e (len ,a))))
                (unit `(substring ,a ,s ,e))
                (panic "substring out of bounds: ~v" `(substring ,a ,s ,e))))]
        [getbyte
         (λ (a b)
           (>> (log "ensure that getbyte has the invariant added here")
               (sif (<rw `(< ,b (len ,a)))
                    (unit `(getbyte ,a ,b))
                    (panic "byte access out of bounds: ~v" `(getbyte ,a ,b)))))]
        [global
         (λ (f)
           (match f
             [(or 'MinTxnFee 'MinBalance 'MaxTxnLife 'GroupSize)
              (unit f)]
             ['ZeroAddress
              (>> (log "The ZeroAddress symbolic constant represents a single value. Make sure the theories respect it.")
                  (unit 'ZeroAddress))]
             ['LogicSigVersion
              (>> ((logic-sig-version>= step-VM) 2 "LogicSigVersion")
                  (get logic-sig-version))]
             ['Round
              (>> ((logic-sig-version>= step-VM) 2 "Round")
                  (unit 'Round))]
             ['LatestTimestamp
              (>> ((logic-sig-version>= step-VM) 2 "LatestTimestamp")
                  (>> (log "LatestTimestamp fails if the timestamp given is less than zero. This is neither under the control of the program, nor tracked by the machine.")
                      (unit 'LatestTimestamp)))]
             ['CurrentApplicationID
              (>> ((logic-sig-version>= step-VM) 2 "CurrentApplicationID")
                  (unit 'CurrentApplicationID))]
             ['CreatorAddress
              ; "Address of the creator of the current application. Fails if no such application is executing."
              (>> ((logic-sig-version>= step-VM) 3 "CreatorAddress")
                  (unit 'CreatorAddress))]
             ['CurrentApplicationAddress
              (>> ((logic-sig-version>= step-VM) 5 "CurrentApplicationAddress")
                  (unit 'CurrentApplicationAddress))]
             ['GroupID
              (>> ((logic-sig-version>= step-VM) 5 "GroupID")
                  (unit 'GroupID))]))]
        [transaction (λ (fi) (group-transaction 'this-group-index fi))]
        group-transaction
        ; transaction-array
        [transaction-array (λ (fi ai) (group-transaction-array 'this-group-index fi ai))]
        group-transaction-array
        [load
         (λ (i)
           (>>= (get scratch-space)
                (λ (ss)
                  (cond
                    [(hash-ref ss i #f) => unit]
                    [else
                     (>> (log "Scratch space slot ~a uninitialized. The Go VM implementation produces 0." i)
                         (unit 0))]))))]
        [store (λ (i x) (update scratch-space (λ (ss) (hash-set ss i x))))]
        [balance (λ (acct) (unit `(balance ,acct)))]
        [min-balance (λ (acct) (unit `(min-balance ,acct)))]
        [app-local-get
         (λ (acct key)
           (>>= (get app-local)
                (letrec ([lookup (match-lambda
                                   [(list)
                                    (unit `(app-local ,acct ,key))]
                                   [(cons `(put ,put-acct ,put-key ,val)
                                          al)
                                    (sif `(∧ (= ,acct ,put-acct)
                                             (= ,key  ,put-key))
                                         (unit val)
                                         (lookup al))])])
                  lookup)))]
        [app-local-put
         (λ (acct key val)
           (update app-local (λ (al) (cons `(put ,acct ,key ,val) al))))]
        [app-local-del
         (λ (acct key)
           (update app-local (λ (al) (cons `(del ,acct ,key) al))))]
        [app-local-get-ex
         (λ (acct app-id key)
           (>> (push `(app-local ,acct ,app-id ,key))
               (push `(app-local-exists ,acct ,app-id ,key))))]
        [app-global-get
         (λ (key)
           (>>= (get app-global)
                (letrec ([lookup (match-lambda
                                   [(list)
                                    (unit `(app-global ,key))]
                                   [(cons `(put ,put-key ,val)
                                          ag)
                                    (sif `(= ,key ,put-key)
                                         (unit val)
                                         (lookup ag))])])
                  lookup)))]
        [app-global-put
         (λ (key val)
           (update app-global (λ (ag) (cons `(put ,key ,val) ag))))]
        [app-global-get-ex
         (λ (app-id key)
           (>> (push `(app-global ,app-id ,key))
               (push `(app-global-exists ,app-id ,key))))]
        [asset-holding-get
         (λ (acct asset x)
           (>>= (match x
                  [0 (unit 'AssetBalance)]
                  [1 (unit 'AssetFrozen)])
                (λ (f)
                  (>> (push `(asset-holding ,acct ,asset ,f))
                      (push `(asset-holding-exists ,acct ,asset ,f))))))]
        [asset-params-get
         (λ (asset x)
           (>>= (match x
                  [0  (unit 'AssetTotal)]
                  [1  (unit 'AssetDecimals)]
                  [2  (unit 'AssetDefaultFrozen)]
                  [3  (unit 'AssetUnitName)]
                  [4  (unit 'AssetName)]
                  [5  (unit 'AssetURL)]
                  [6  (unit 'AssetMetadataHash)]
                  [7  (unit 'AssetManager)]
                  [8  (unit 'AssetReserve)]
                  [9  (unit 'AssetFreeze)]
                  [10 (unit 'AssetClawback)]
                  [11 (unit 'AssetCreator)])
                (λ (f)
                  (>> (push `(asset-params ,asset ,f))
                      (push `(asset-params-exists ,asset ,f))))))]
        [bzero (λ (x) (unit `(bzero ,x)))]
        [check-final
         (>>= (get bytecode)
              (λ (bc)
                (>>= (get pc)
                     (λ (pc)
                       (if (= pc (bytes-length bc))
                         (>>= (get stack)
                              (match-lambda
                                [(list) (panic "stack is empty at end of program")]
                                [(list x) (return x)]
                                [_ (panic "stack has more than one value at end of program")]))
                         (unit))))))])))

#;
(require racket/pretty)

(define (inject lsv bs)
  (hasheq 'logic-sig-version lsv
          'bytecode bs
          'pc 0
          'stack (list)
          'intcblock (list)
          'bytecblock (list)
          'scratch-space (hasheqv)
          'path-condition (set)
          'app-local (list) ; at best a map, at worst a sequence of puts
          'app-global (list)
          'execution-log (list)
          'call-stack (list)))

(define (run bytecode)
  (match ((read-varuint prefix-ReadByte) bytecode)
    [(cons lsv bs)
     ; until there are potential loops,
     ; this does not need to be a fixed point calculation.
     (let loop ([st (inject lsv bs)]
                [fs (set)])
       (foldl
        (λ (r fs)
          (match r
            [(underway (list) st)
             (loop st fs)]
            [(failure! msg)
             fs]
            [(returned code)
             (if (zero? code)
               fs
               (set-add fs (cons code st)))]))
        fs
        ((step step-VM) st)))]
    [#f
     (error 'analyze "unable to read initial logic signature version")]))



(module+ main
  (require racket/port
           racket/pretty)

  (for-each
   (match-lambda
     [(cons code st)
      (displayln code)
      (pretty-print (hash-remove st 'bytecode))])
   (set->list (run (port->bytes (current-input-port))))) 

  #;
  (match (current-command-line-arguments)
    [(vector filename)
     (displayln filename)
     (match ((read-prefix read-ReadByte) (file->bytes filename))
       [(cons lsv bs)
        ])]))

#;
(= (hi (mulw (+ (+ (txn 4 AssetAmount) 1000) 1)
             (+ (+ (txn 4 AssetAmount) 1000) 1)))
   (hi (mulw (+ (txn 2 AssetAmount) (txn 2 Amount))
             (+ (txn 3 AssetAmount) (txn 3 Amount)))))

#;
(< (lo (mulw (+ (txn 2 AssetAmount) (txn 2 Amount))
             (+ (txn 3 AssetAmount) (txn 3 Amount))))
   (lo (mulw (+ (+ (txn 4 AssetAmount) 1000) 1)
             (+ (+ (txn 4 AssetAmount) 1000) 1))))
