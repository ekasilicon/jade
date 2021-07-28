#lang racket/base
(require racket/match
         racket/set)

(struct environment (mode globals transaction transaction-group scratch-space) #:transparent)
(struct state (pc stack scratch-space intcblock bytecblock execution-cost) #:transparent)

(struct partial (values environment state) #:transparent)
(struct failure (message environment) #:transparent)
(struct success (code environment) #:transparent)

(define ((unit . xs) r st) (list (partial xs r st)))
(define ((fail template . args) r st) (list (failure (apply format template args) r)))
(define ((succeed code) r st) (list (success code r)))

(define (append-map f xs)
  (match xs
    [(list) (list)]
    [(cons x xs)
     (append (f x) (append-map f xs))]))

(define ((>>= m f) r st)
  (append-map
   (match-lambda
     [(partial xs r st)
      ((apply f xs) r st)]
     [x (list x)])
   (m r st)))

(define (>> m . ms)
  (foldl (λ (m M) (>>= M (λ _ m))) m ms))

(define ((each . ms) r st)
  (append-map (λ (m) (m r st)) ms))

(define ((lift f) . xs) (unit (apply f xs)))

(define (ask-env r st)
  (list (partial (list r) r st)))

(define ((put-env r) _ st)
  (list (partial (list) r st)))

(define (get r st)
  (list (partial (list st) r st)))

(define ((put st) r _)
  (list (partial (list) r st)))

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax get-field
  (syntax-parser
    #:datum-literals (pc stack intcblock bytecblock)
    [(_ pc)
     #'(>>= get (lift state-pc))]
    [(_ stack)
     #'(>>= get (lift state-stack))]
    [(_ intcblock)
     #'(>>= get (lift state-intcblock))]
    [(_ bytecblock)
     #'(>>= get (lift state-bytecblock))]))

(define-syntax put-field
  (syntax-parser
    #:datum-literals (pc stack intcblock bytecblock)
    [(_ pc v)
     #'(>>= get (λ (st) (put (struct-copy state st [pc v]))))]
    [(_ stack v)
     #'(>>= get (λ (st) (put (struct-copy state st [stack v]))))]
    [(_ intcblock v)
     #'(>>= get (λ (st) (put (struct-copy state st [intcblock v]))))]
    [(_ bytecblock v)
     #'(>>= get (λ (st) (put (struct-copy state st [bytecblock v]))))]))

(define-syntax update-field
  (syntax-parser
    #:datum-literals (pc stack scratch-space)
    [(_ pc g)
     #'(>>= get (λ (st) (put (struct-copy state st [pc (g (state-pc st))]))))]
    [(_ stack g)
     #'(>>= get (λ (st) (put (struct-copy state st [stack (g (state-stack st))]))))]
    [(_ scratch-space g)
     #'(>>= get (λ (st) (put (struct-copy state st [scratch-space (g (state-scratch-space st))]))))]))
















(define (transaction key [privileged? #f])
  (match key
    ['TypeEnum
     #;16
     (if privileged?
       (>>= ask-env
            (λ (r)
              (let ([txn (environment-transaction r)])
                (cond
                  [(hash-ref txn 'TypeEnum #f)
                   => unit]
                  [else
                   (apply each
                          (map
                           (λ (type-enum)
                             (>> (put-env (struct-copy environment r [transaction (hash-set txn 'TypeEnum type-enum)]))
                                 (unit type-enum)))
                           (list 0 1 2 3 4 5 6)))]))))
       (stub "TypeEnum"))
     ]
    ['OnCompletion
     #;25
     (if privileged?
       (>>= ask-env
            (λ (r)
              (let ([txn (environment-transaction r)])
                (cond
                  [(hash-ref txn 'OnCompletion #f)
                   => unit]
                  [else
                   (apply each
                          (map
                           (λ (on-completion)
                             (>> (put-env (struct-copy environment r [transaction (hash-set txn 'OnCompletion on-completion)]))
                                 (unit on-completion)))
                           (list '(≠ 3)
                               '(= 3))))]))))
       (stub "OnCompletion"))]
    [(or 'ApprovalProgram 'ClearStateProgram)
     #;30 #;31
     ; can assume the programs are present
     (if privileged?
       (>>= (>>= ask-env (lift environment-transaction))
            (λ (txn) (unit (hash-ref txn key))))
       (stub (symbol->string key)))]
    ['RekeyTo
     #;32
     (if privileged?
       (>>= ask-env
            (λ (r)
              (let ([txn (environment-transaction r)])
                (cond
                  [(hash-ref txn 'RekeyTo #f)
                   => unit]
                  [else
                   (unit `(field RekeyTo))]))))
       (stub (symbol->string key)))
     ])
  #;
  (>>= (transaction 'GroupIndex #t)
       (λ (i) (transactions i key privileged?))))

(define (transactions i key [privileged? #f])
  (stub "transactions")
  #;
  (match (list i key privileged?))
  #;
  (>>= (>>= ask-env (lift environment-transaction-group))
       (λ (txn-group)
         (define (guarded-access min-lsv)
           (if privileged?
             (txn-group i key)
             (>>= logic-sig-version
                  (λ (lsv)
                    (if (>= lsv min-lsv)
                      (txn-group i key)
                      (fail "transaction field ~a requires version at least ~a; using ~a" key min-lsv lsv))))))
         (cond
           [(memq key '(Sender Fee FirstValid FirstValidTime LastValid Note Lease Receiver Amount CloseRemainderTo VotePK SelectionPK VoteFirst VoteLast VoteKeyDilution Type TypeEnum XferAsset AssetAmount AssetSender AssetReceiver AssetCloseTo GroupIndex TxID))
            (guarded-access 1)]
           [(memq key '(ApplicationID OnCompletion ApplicationArgs NumAppArgs Accounts NumAccounts ApprovalProgram ClearStateProgram RekeyTo
                        ConfigAsset ConfigAssetTotal ConfigAssetDecimals ConfigAssetDefaultFrozen ConfigAssetUnitName ConfigAssetName ConfigAssetURL ConfigAssetMetadataHash ConfigAssetManager ConfigAssetReserve ConfigAssetFreeze ConfigAssetClawback
                        FreezeAsset FreezeAssetAccount FreezeAssetFrozen))
            (guarded-access 2)]
           [(memq key '(Assets NumAssets Applications NumApplications GlobalNumUint GlobalNumByteSlice LocalNumUint LocalNumByteSlice))
            (guarded-access 3)]
           [(memq key '(ExtraProgramPages))
            (guarded-access 4)]
           [else
            (error 'interpret "unknown transaction field index ~a" key)]))))

(define (global key [privileged? #f])
  (>>= (>>= ask-env (lift environment-globals))
       (λ (globals)
         (match key
           ['GroupSize
            #;4
            (cond
              [(hash-ref globals 'GroupSize #f)
               => unit]
              [else
               (unit `(global GroupSize))])]
           ['LogicSigVersion
            (>> (introduced 2 "LogicSigVersion")
                (unit (hash-ref globals 'LogicSigVersion)))])
         #;
         (globals key privileged?))))

(define logic-sig-version
  (global 'LogicSigVersion #t))





(define get-pc (get-field pc))

(define program-bytes
  (>>= (transaction 'OnCompletion #t)
       (match-lambda
         [`(= 3)
          (transaction 'ClearStateProgram #t)]
         [`(≠ 3)
          (transaction 'ApprovalProgram #t)])))

(define program-length (>>= program-bytes (lift bytes-length)))

(define read-byte
  (>>= get-pc
       (λ (pc-val)
         (if (< pc-val 0)
           (fail "attempt to read negative index at ~a" pc-val)
           (>>= program-bytes
                (λ (bs)
                  (if (>= pc-val (bytes-length bs))
                    (fail "attempt to read past end of program of ~a bytes at ~a" (bytes-length bs) pc-val)
                    (>> (update-field pc add1)
                        (unit (bytes-ref bs pc-val))))))))))

(define read-opcode read-byte)

(define read-uint8 read-byte)

(define read-int16
  (>>= read-byte
       (λ (bu)
         (>>= read-byte
              (λ (bl)
                (let ([x (+ (* 256 bu) bl)])
                  (unit (if (< x (expt 2 15))
                          x
                          (- x (exp 2 16))))))))))

(define read-varuint
  (>>= read-byte
       (λ (b)
         (if (zero? (bitwise-and b #x80))
           (unit b)
           (>>= read-varuint (λ (n) (unit (+ b (* n 128)))))))))

(define read-bytes
  (>>= (>>= read-varuint
          (λ (n)
            (let loop ([n n])
              (if (zero? n)
                (unit (list))
                (>>= read-byte
                     (λ (b)
                       (>>= (loop (sub1 n))
                            (λ (bs) (unit (cons b bs))))))))))
       (λ (bs) (unit (apply bytes bs)))))


(define (push x)
  (update-field stack (λ (stk) (cons x stk))))

(define (pop n)
  (>>= (get-field stack)
       (λ (stk)
         (let loop ([n n]
                    [stk stk]
                    [xs (list)])
           (if (zero? n)
             (>> (put-field stack stk)
                 (apply unit xs))
             (match stk
               [(list)
                (fail "popping an empty stack")]
               [(cons x stk)
                (loop (sub1 n)
                      stk
                      (cons x xs))]))))))

(define-syntax-rule (datum-case x m₀ m₁)
  (cond
    [(exact-nonnegative-integer? x) m₀]
    [(bytes? x) m₁]
    [else
     (match x
       [`(global GroupSize)
        m₀]
       [`(address ,taint)
        m₁])]))

(define (iszero x)
  (unit (zero? x)))

(define (is< x y)
  (unit `(< ,x ,y)))

(define (is=/u x y)
  (unit `(=/u ,x ,y)))

(define (is=/b x y)
  (unit `(=/b ,x ,y)))

(define (execute-al bc)
  (match bc
    [#x0c ; <
     (>>= (pop 2)
          (λ (B A)
            (datum-case A
              (datum-case B
                (>>= (is< B A) push)
                (fail "< requires two uint64s; left operand is ~v" B))
              (fail "< requires two uint64s; right operand is ~v" A))))]
    ; the following three instructions are implemented as in the Go VM
    ; but I might need to specialize them to deal with the abstraction
    ; (but it would be good if the the abstraction broke on these
    ; contours)
    [#x0d ; >
     (>> (execute-opcode #x4c)   ; swap
         (execute-opcode #x0c))] ; <
    [#x0e ; <=
     (>> (execute-opcode #x0d)   ; >
         (execute-opcode #x14))] ; !
    [#x0f ; >=
     (>> (execute-opcode #x0c)   ; <
         (execute-opcode #x14))] ; !
    [#x12 ; ==
     (>>= (pop 2)
          (λ (B A)
            (datum-case A
              (datum-case B
                (>>= (is=/u A B) (λ (r) (push (if r 1 0))))
                (fail "cannot compare uint64 and bytes for equality"))
              (datum-case B
                (fail "cannot compare bytes and uint64 for equality")
                (>>= (is=/b A B) (λ (r) (push (if r 1 0))))))))]
    [#x14 ; !
     (>>= (pop 1)
          (λ (A)
            (datum-case A
              (push (if (zero? A) 1 0))
              (fail "! expects uint64; got ~v" A))))]))


(define (execute-intc bc)
  (define (intcblock-save intcb)
    (put-field intcblock intcb))
  (define (intcblock-load i)
    (>>= (get-field intcblock)
         (λ (intcs)
           (if (< i (length intcs))
             (unit (list-ref intcs i))
             (fail "there are ~a constant integers registered but index ~a requested" (length intcs) i)))))
  (define (intcblock-load/push i)
    (>>= (intcblock-load i) push))
  (match bc
    [#x20 ; intcblock
     (>>= (>>= read-varuint
               (λ (n)
                 (let loop ([n n])
                   (if (zero? n)
                     (unit (list))
                     (>>= read-varuint
                          (λ (x)
                            (>>= (loop (sub1 n))
                                 (λ (xs)
                                   (unit (cons x xs))))))))))
          intcblock-save)]
    [#x21 ; intc
     (>>= read-uint8 intcblock-load/push)]
    [#x22 ; intc_0
     (intcblock-load/push 0)]
    [#x23 ; intc_1
     (intcblock-load/push 1)]
    [#x24 ; intc_2
     (intcblock-load/push 2)]
    [#x25 ; intc_3
     (intcblock-load/push 3)]))

(define (execute-bytec bc)
  (define (bytecblock-save bytecb)
    (put-field bytecblock bytecb))
  (define (bytecblock-load i)
    (>>= (get-field bytecblock)
         (λ (bytecs)
           (if (< i (length bytecs))
             (unit (list-ref bytecs i))
             (fail "there are ~a constant bytes registered but index ~a requested" (length bytecs) i)))))
  (define (bytecblock-load/push i)
    (>>= (bytecblock-load i) push))
  (match bc
    [#x26 ; bytecblock
     (>>= (>>= read-varuint
             (λ (n)
               (let loop ([n n])
                 (if (zero? n)
                   (unit (list))
                   (>>= read-bytes
                        (λ (x)
                          (>>= (loop (sub1 n))
                               (λ (xs) (unit (cons x xs))))))))))
          bytecblock-save)]
    [#x27 ; bytec
     (>>= read-uint8 bytecblock-load/push)]
    [#x28 ; bytec_0
     (bytecblock-load/push 0)]
    [#x29 ; bytec_1
     (bytecblock-load/push 1)]
    [#x2a ; bytec_2
     (bytecblock-load/push 2)]
    [#x2b ; bytec_3
     (bytecblock-load/push 3)]))

(define (transaction-index→key i)
  (define keys
    '(Sender Fee FirstValid FirstValidTime LastValid Note Lease Receiver Amount CloseRemainderTo VotePK SelectionPK VoteFirst VoteLast VoteKeyDilution Type TypeEnum XferAsset AssetAmount AssetSender AssetReceiver AssetCloseTo GroupIndex TxID ApplicationID OnCompletion ApplicationArgs NumAppArgs Accounts NumAccounts ApprovalProgram ClearStateProgram RekeyTo
      ConfigAsset ConfigAssetTotal ConfigAssetDecimals ConfigAssetDefaultFrozen ConfigAssetUnitName ConfigAssetName ConfigAssetURL ConfigAssetMetadataHash ConfigAssetManager ConfigAssetReserve ConfigAssetFreeze ConfigAssetClawback
      FreezeAsset FreezeAssetAccount FreezeAssetFrozen Assets NumAssets Applications NumApplications GlobalNumUint GlobalNumByteSlice LocalNumUint LocalNumByteSlice ExtraProgramPages))
  (if (< i (length keys))
    (unit (list-ref keys i))
    (fail "transaction index ~a out of range" i)))

(define (execute-env bc)
  (match bc
    [#x31 ; txn
     (>>= read-uint8
          (λ (i) (>>= (>>= (transaction-index→key i) transaction) push)
             #;
             (match i
               [25                      ; OnCompletion
                (>> (introduced 2 "OnCompletion")
                    (>>= (unit 'NoOp)
                                        ; actually read the op
                         (λ (op)
                           (push ((match-lambda
                                    ['NoOp 0]
                                    ['OptIn 1]
                                    ['CloseOut 2]
                                    ['ClearState 3]
                                    ['UpdateApplication 4]
                                    ['DeleteApplication 5])
                                  op)))))]
               [32                      ; ReykeyTo (bytes, address)
                (>> (introduced 2 "RekeyTo")
                                        ; read 'RekeyTo
                    (push (make-bytes 32 0)))])))]
    [#x32                               ; global
     (define (global-index→key i)
       (define keys
         '(MinTxnFee MinBalance MaxTxnLife ZeroAddress GroupSize LogicSigVersion Round LatestTimestamp CurrentApplicationID CreatorAddress))
       (if (< i (length keys))
         (unit (list-ref keys i))
         (fail "global index ~a out of range" i)))
     (>>= (>>= (>>= read-uint8 global-index→key)
               global)
          push)]
    [#x35 ; store
     (>>= read-uint8
          (λ (i)
            (>>= (pop 1)
                 (λ (x) (update-field scratch-space (λ (ss) (hash-set ss i x)))))))]
    [#x37 ; gtxna
     (>> (introduced 2 "gtxna")
         (>>= read-byte
              (λ (group-index)
                (>>= read-byte
                     (λ (field-index)
                       (>>= read-byte
                            (λ (i)
                              (>>= (transaction-index→key field-index)
                                   (λ (field-key)
                                     (>>= (transactions group-index field-key)
                                          (λ (args)
                                            (if (< i (length args))
                                              (push (list-ref args i))
                                              (fail "ApplicationArgs index ~a exceeds number ~a" i (length args))))))))))))))]))

(define (execute-ctl bc)
  (match bc
    [#x40 ; bnz
     (>>= read-int16
          (λ (offset)
            (>>= (pop 1)
                 (λ (x)
                   (>>= (iszero x) (λ (z?) (if z? (unit) (jump offset))))))))]
           
    [#x41 ; bz
     (>> (introduced 2 "bz")
         (>>= read-int16
              (λ (offset)
                (>>= (pop 1)
                     (λ (x)
                       (>>= (iszero x) (λ (z?) (if z? (jump offset) (unit)))))))))]
    [#x42 ; b
     (>> (introduced 2 "b")
         (>>= read-int16 jump))]
    [#x43 ; return
     (>> (introduced 2 "return")
         (>>= (pop 1) succeed))]
    [#x44 ; assert
     (>> (introduced 3 "assert")
         (>>= (pop 1)
              (λ (x)
                (datum-case x
                  (>>= (iszero x) (λ (z?) (if z? (fail "assert failed") (unit))))
                  (fail "assert expects uint64 but got []byte")))))]))

(define (unimplemented bc)
  (fail "need to implement ~a" (number->string bc 16)))

(define execute-hash unimplemented)
(define execute-push unimplemented)
(define execute-app unimplemented)
(define execute-params unimplemented)
(define execute-extract unimplemented)
(define execute-stk unimplemented)
(define execute-arg unimplemented)
(define execute-aw unimplemented)
(define execute-call unimplemented)
(define execute-byteop unimplemented)

(define (unused bc)
  (fail "unrecognized instruction ~a" (number->string bc 16)))

(define (stub msg)
  (fail "stub: ~a" msg))

(define (execute-opcode bc)
  (define-match-expander range
    (syntax-rules ()
      [(_ lo hi)
       (? (λ (bc) (<= lo bc hi)))]))
  (match bc
    [#x00 ; fail
     (fail "err instruction")]
    [(range #x01 #x04)
     (execute-hash bc)]
    [(range #x05 #x07)
     (unused bc)]
    [(range #x08 #x1c)
     (execute-al bc)]
    [(range #x1d #x1f)
     (execute-aw bc)]
    [(range #x20 #x25)
     (execute-intc bc)]
    [(range #x26 #x2b)
     (execute-bytec bc)]
    [(range #x2c #x30)
     (execute-arg bc)]
    [(range #x31 #x3d)
     ; this is significant
     ; and should probably be factored
     (execute-env bc)]
    [(range #x3e #x3f)
     (unused bc)]
    [(range #x40 #x44)
     (execute-ctl bc)]
    [(range #x45 #x47)
     (unused bc)]
    [(range #x48 #x4d)
     (execute-stk bc)]
    [(range #x4e #x4f)
     (unused bc)]
    [(range #x50 #x5b)
     ; this is significant
     (execute-extract bc)]
    [(range #x60 #x69)
     ; this is significant
     (execute-app bc)]
    [(range #x6a #x6f)
     (unused bc)]
    [(range #x70 #x72)
     (execute-params bc)]
    [#x78 ; min_balance
     (execute-app bc)]
    [(range #x79 #x7f)
     (unused bc)]
    [(range #x80 #x81)
     (execute-push bc)]
    [(range #x82 #x87)
     (unused bc)]
    [(range #x88 #x89)
     (execute-call bc)]
    [(range #x8a #x8f)
     (unused bc)]
    [(range #x90 #x94)
     (execute-al bc)]
    [#x95 ; expw
     (execute-aw bc)]
    [(range #x96 #x9f)
     (unused bc)]
    [(range #xa0 #xaf)
     (execute-byteop bc)]
    [(range #xb0 #xff)
     (unused bc)]))

(require racket/pretty)
(define →
  (>> (>>= get (lift pretty-print))
      (>>= read-opcode execute-opcode)
      (>>= get-pc
           (λ (pc)
             (>>= program-length
                  (λ (n)
                    (if (= pc n)
                      (>>= (get-field stack)
                           (match-lambda
                             [(list code) (succeed code)]
                             [stk (fail "reached end of program but ~a values on stack" (length stk))]))
                      (unit))))))))

(define (introduced lsv name)
  (>>= logic-sig-version
       (λ (current-lsv)
         (if (>= current-lsv lsv)
           (unit)
           (fail "~a introduced in version ~a; not available in version ~a" name lsv current-lsv)))))




(define (goto pc)
  (if (< pc 0)
    (fail "destination ~a before start of program" pc)
    (>>= program-length
         (λ (n)
           (if (> pc n)
             (fail "destination ~a strictly after end of program" pc)
             (>>= logic-sig-version
                  (λ (lsv)
                    (if (or (< pc n)
                            (>= lsv 2))
                      (put-field pc pc)
                      (fail "destination ~a at end of program in version 1" pc)))))))))

(define (jump offset)
  (>>= logic-sig-version
       (λ (lsv)
         (if (and (< offset 0)
                  (< lsv 4))
           (fail "negative offset ~a not permitted in version ~a" offset lsv)
           (>>= get-pc (λ (pc) (goto (+ pc offset))))))))

(define (initialize teal)
  (if (zero? (bytes-length teal))
    (error 'interpret "empty program")
    (let ([lsv (bytes-ref teal 0)]
          [pgm (subbytes teal 1)])
      (if (zero? (bitwise-and lsv #x80))
        (let (#;
              [txn-group (list (hasheq #;16 'TypeEnum 6
                                       #;22 'GroupIndex 0
                                       #;26 'ApplicationArgs (list #"" #"" #"" #"" #"" #"" #"" #"" #"")))])
          (environment 'LogicSig
                       (hasheq 'LogicSigVersion lsv)
                       (hasheq 'ApprovalProgram pgm
                               'ClearStateProgram #"")
                       (hasheq)
                       #;
                       (λ (key privileged?)
                         (define (access min-lsv m)
                           (if (or privileged?
                                   (>= lsv min-lsv))
                             m
                             (fail "global ~a requires version at least ~a; using ~a" key min-lsv lsv)))
                         (match key
                           #;MinTxnFee
                           #;MinBalance
                           #;MaxTxnLife
                           ['ZeroAddress
                            (access 1 (unit (bytes->immutable-bytes (make-bytes 32 0))))]
                           ['GroupSize
                            (access 1 (unit (length txn-group)))]
                           ['LogicSigVersion
                            (access 2 (unit lsv))]
                           ['Round
                            (access 2 (unit 'Round))]
                           #;'LatestTimestamp
                           #;CurrentApplicationID
                           #;CreatorAddress
                         ))
                       #;
                       'txn-group
                       #;
                       (λ (i key)
                         (if (< i (length txn-group))
                           (let ([txn (list-ref txn-group i)])
                             (match key
                               ['Sender
                                #;0
                                (unit '(address Sender))]
                               ['Type
                                #;15
                                (unit (match (hash-ref txn 'TypeEnum)
                                        [0 #"unknown"]
                                        [1 #"pay"]
                                        [2 #"keyreg"]
                                        [3 #"acfg"]
                                        [4 #"axfer"]
                                        [5 #"afrz"]
                                        [6 #"appl"]))]
                               ['TypeEnum
                                #;16
                                (unit (hash-ref txn 'TypeEnum))]
                               ['ApplicationID]
                               ['OnCompletion
                                #;25
                                (unit 0)]
                               ['ApplicationArgs
                                (unit (hash-ref txn 'ApplicationArgs))]
                               ['NumAppArgs
                                #;27
                                (unit (length (hash-ref txn 'ApplicationArgs)))]
                               ['ApprovalProgram
                                #;30
                                (unit pgm)]
                               ['RekeyTo
                                #;32
                                (unit (bytes->immutable-bytes (make-bytes 32 0)))]))
                           (fail "gtxn lookup TxnGroup[~a] but it only has ~a" i (length txn-group))))
                       (hasheq)))
        (error 'initialize "version spills into more than one byte")))))

(define (execute teal-bytes)
  (let →* ([r (initialize teal-bytes)]
           [st (state 0 (list) (hasheqv) (list) (list) 0)]
           [finals (set)])
    (let loop ([xs (→ r st)]
               [finals finals])
      (match xs
        [(list)
         finals]
        [(cons x xs)
         (loop xs
               (match x
                 [(partial (list) r st)
                  (→* r st finals)]
                 [x
                  (set-add finals x)]))]))))

(provide success failure execute)

(module+ main
  (require racket/file)
  (match (current-command-line-arguments)
    [(vector filename)
     (execute (file->bytes filename))]))






#|

(define (mode expected-mode)
  (>>= (>>= ask-env (lift environment-mode))
       (λ (actual-mode)
         (if (eq? expected-mode actual-mode)
           (unit)
           (fail "expected mode: ~a; actual mode: ~a" expected-mode actual-mode)))))

(define ((consume n) r st)
  (let ([ec (+ (state-execution-cost st) n)]
        [lsv (list-ref (hash-ref r 'globals) 5)])
    ((stub "consume") r st)))

(define (cost n) (consume (sub1 n)))


(define (op-hash bc)
  (define (hashop hash-f cost-f)
    (>>= (pop 1)
         (λ (ba)
           (>> (push (list hash-f ba))
               (>>= logic-sig-version
                    (λ (lsv) (cost (cost-f lsv))))))))
  (define (v1-cost m n)
    (match-lambda
      [(or 1)     m]
      [(or 2 3 4) n]))
  (match bc
    [#x01 ; sha256
     (hashop 'sha256 (v1-cost 7 35))]
    [#x02 ; keccak256
     (hashop 'keccak256 (v1-cost 26 130))]
    [#x03 ; sha512_256
     (hashop 'sha512-256 (v1-cost 9 45))]
    [#x04 ; ed25519verify
     (>> (mode 'LogicSig)
         (>>= (pop 3)
              (λ (C B A)
                (>> (cost 1900)
                    (stub "ed25519verify")))))]))

(define (op-arith bc)
  
  #|
  (define ((make-op op) A B) (unit (op A B)))
  (define safe+ (make-op +))
  (define safe- (make-op -))
  (define safe* (make-op *))
  (define safe/ (make-op /))
  (define safe< (make-op (λ (A B) (if (< A B) 1 0))))
  ; the operation can fail
  (define (binop op-m)
    (>>= (pop 2) (λ (B A) (>>= (op-m A B) push))))
  (define (op2 name op-m) (binop op-m))
  (define safe&& (make-op (λ (A B) (if (or (zero? A) (zero? B)) 0 1))))
  (define safelor (make-op (λ (A B) (if (and (zero? A) (zero? B)) 0 1))))
  (define safe!= (make-op (λ (A B) (if (= A B) 0 1))))
  (define (op1 name op-m)
    (>>= (pop 1) (λ (A) (>>= (op-m A) push))))
  (define safe! (op1 ""(λ (A) (>>= (iszero A) (λ (z?) (unit (if z? 1 0)))))))
  (define safelen (op1 (λ (A)
                         (if (bytes? A)
                           (unit (bytes-length A))
                           (fail "len: expected bytes but got ~v" A)))))
  (define safeitob (op1 (λ (A)
                          (if (exact-nonnegative-integer? A)
                            (stub "itob")
                            (fail "itob: expected uint64 but got ~v" A)))))
  (define safebtoi (op1 (λ (A)
                          (if (bytes? A)
                            (if (<= (bytes-length A) 8)
                              (stub "btoi")
                              (fail "btoi: bytes with length ~a not 8 or less" (bytes-length A)))
                            (fail "btoi: expected bytes but got ~v" A)))))
  |#
  #;
  (match bc
    [#x08 ; +
     (binop safe+)]
    [#x09 ; -
     (binop safe-)]
    [#x0a ; /
     (binop safe/)]
    [#x0b ; *
     (binop safe*)]
    [#x10 ; &&
     (op2 "&&" safe&&)]
    [#x11 ; ||
     (op2 "||" safelor)]
    
    [#x13 ; !=
     (op2 "!=" safe!=)]
    
    [#x15 ; len
     (op1 #;"len" safelen
          )]
    [#x16 ; itob
     (op1 #;"itob" safeitob
          )]
    [#x17 ; btoi
     (op1 #;"btoi" safebtoi
          )]
    #;
    [#x18 ; %
     (op2 "%" safe%)]
    #;
    [#x19 ; |
     (op2 "|" safelor)]
    #;
    [#x1a ; &
     (op2 "&" safe&)]
    #;
    [#x1b ; ^
     (op2 "^" safe^)]
    #;
    [#x1c ; ~
     (op1 "~" safe~)]))

(define (op-arithw bc)
  (match bc)
  #;
  (define (w+ A B) (+ A B))
  #;
  (match bc
    [#x1d ; mulw
     (stub "mulw")]
    [#x1e ; addw
     (>> (introduced 2 "addw")
         (>>= (pop 2)
              (λ (B A)
                (let-values ([(sum carry) (w+ B A)])
                  (>> (push carry)
                      (push sum))))))]
    [#x1f ; divmodw
     (>> (introduced 4 "divmodw")
         (cost 20)
         (>>= (pop 4)
              (λ (D C B A)
                (stub "divmodw"))))]))



(define (iszero x) (unit (zero? x)))




(define (op-argument bc)
  (define (argument-load i)
    (>>= (transaction 'NumAppArgs #t)
         (λ (n)
           (if (< i n)
             (>>= (transaction 'ApplicationArgs #t)
                  (λ (bs)
                    (stub "argument-load")))
             (fail "argument ~a requested but ~a argument(s) given" i n)))))
  (define (argument-load/push n)
    (>>= (argument-load n) push))
  (>> (mode 'LogicSig)
      (match bc
        [#x2c ; arg
         (>>= read-uint8 argument-load/push)]
        [#x2d ; arg_0
         (argument-load/push 0)]
        [#x2e ; arg_1
         (argument-load/push 1)]
        [#x2f ; arg_2
         (argument-load/push 2)]
        [#x30 ; arg_3
         (argument-load/push 3)])))

(define (op-stack bc)
  (match bc
    [#x48 ; pop
     (pop 1)]
    [#x49 ; dup
     (>>= (pop 1)
          (λ (x)
            (>> (push x)
                (push x))))]
    [#x4a ; dup2
     (>> (introduced 2 "dup2")
         (>>= (pop 2)
              (λ (B A)
                (>> (push A)
                    (push B)
                    (push A)
                    (push B)))))]
    [#x4b ; dig
     (>> (introduced 3 "dig")
         (>>= read-uint8
              (λ (n)
                (let loop ([n n]
                           [xs (list)])
                  (>>= (pop 1)
                       (λ (x)
                         (if (zero? n)
                           (>> (push x)
                               (let loop ([xs xs])
                                 (match xs
                                   [(list)
                                    (unit)]
                                   [(cons x xs)
                                    (>> (push x)
                                        (loop xs))]))
                               (push x))
                           (loop (sub1 n) (cons x xs)))))))))]
    [#x4c ; swap
     (>> (introduced 3 "swap")
         (>>= (pop 2)
              (λ (B A)
                (>> (push B)
                    (push A)))))]
    [#x4d ; select
     (>> (introduced 3 "select")
         (>>= (pop 3)
              (λ (C B A)
                (>>= (zero? C) (λ (z?) (push (if z? A B)))))))]))


(define (op-balance bc)
  (define Txn.Accounts (stub "Txn.Accounts"))
  (define (account-address id)
    (datum-case id
                (>>= Txn.Accounts
                     (λ (accts)
                       (if (< id (length accts))
                         (unit (list-ref accts id))
                         (fail "Txn.Accounts index ~a but ~a accounts" id (length accts)))))
                (>>= logic-sig-version
                     (λ (lsv)
                       (if (< lsv 4)
                         (fail "literal address not acceptable in version ~a" lsv)
                         ; make sure it's in Txn.Accounts or is Txn.Sender
                         (stub "balance"))))))
  (match bc
    [#x60 ; balance
     (>> (introduced 2 "balance")
         (mode 'Application)
         (>>= (pop 1)
              (λ (A)
                (>>= (account-address A)
                     (λ (acct) (stub "balance"))))))]
    [#x78
     (>> (introduced 3 "min_balance")
         (mode 'Application)
         (>>= (pop 1)
              (λ (A)
                (>>= (account-address A)
                     (λ (acct) (stub "balance"))))))]))




#;
(define (execute-opcode bc)
  (printf "execute-opcode: ~a\n" (number->string bc 16))
  (match bc
    [#x00 ; err
     (fail "zero byte as instruction")]
    [(range #x01 #x04)
     (op-hash bc)]
    [(range #x05 #x07)
     (unused bc)]
    [(range #x08 #x1c)
     (op-arith bc)]
    [(range #x1d #x1f)
     (op-arithw bc)]
    [(range #x20 #x25)
     (op-intc bc)]
    [(range #x26 #x2b)
     (op-bytec bc)]
    [(range #x2c #x30)
     (op-argument bc)]
    #|
                                        ;
                                        ; ... ;
    [#x3d                       ; gaids ;
    (>> (introduced 4 "gaids")          ;
    (mode 'Application)                 ;
    (>>= (pop 1)                        ;
    (λ (x)                              ;
                                        ; push id of xth asset or application in current group ;
    (stub "gaids"))))]                  ;
    |#
    [(range #x48 #x4d)
     (op-stack bc)]
    #|
                                        ;
    [#x50                       ; concat ;
    (>> (introduced 2 "concat")         ;
    (stub "concat"))]                   ;
                                        ; ... ;
    [#x53                       ; getbit ;
    (>> (introduced 3 "getbit")         ;
    (>> (>>= (pop 2)                    ;
    (λ (n x)                            ;
    (stub "getbit")))                   ;
    next))]                             ;
    [#x54                       ; setbit ;
    (>> (introduced 3 "setbit")         ;
    (>> (>>= (pop 3)                    ;
    (λ (C B A)                          ;
    (stub "setbit")))                   ;
    next))]                             ;
                                        ; ... ;
    [#x60                       ; balance ;
    (>> (introduced 2 "balance")        ;
    (mode 'Application)                 ;
    (>>= (>>= (pop 1)                   ;
    (λ (A)                              ;
    (stub "balance")))))]               ;
                                        ; ... ;
    [#x64                       ; app_global_get ;
    (>> (introduced 2 "app_global_get") ;
    (>> (>>= (pop 1)                    ;
    (λ (A)                              ;
    (stub "app_global_get")))           ;
    next))]                             ;
                                        ; ... ;
    [#x69                       ; app_global_del ;
    (>> (introduced 2 "app_global_del") ;
    (mode 'Application)                 ;
    (>> (>>= (pop 1)                    ;
    (λ (A)                              ;
    (stub "app_global_del")))           ;
    next))]                             ;
                                        ; ... ;
    [#x78                       ; min_balance ;
    (>> (introduced 3 "min_balance")    ;
    (mode 'Application)                 ;
    (>> (>>= (pop 1)                    ;
    (λ (A)                              ;
    (>>= logic-sig-version              ;
    (λ (lsv)                            ;
    (push (if (< lsv 4)                 ;
    (A is a Txn.Accounts offset)        ;
    (offset or address in Txn.Accounts or Txn.Sender))))))) ;
    next))]                             ;
    |#
    #;
    [(range #x61 #x69)
     (op-appdata bc)]
    [(or #x60                           ; balance
         #x78)                          ; min_balance
     (op-balance bc)]
    [(range #x79 #x7f)
     (unused bc)]
    [#x80                               ; pushbytes
     (>> (introduced 3 "pushbytes")
         (>>= read-bytes push))]
    [#x81                               ; pushint
     (>> (introduced 3 "pushint")
         (>>= read-varuint push))]
    [(range #x82 #x87)
     (unused bc)]
    #|
                                        ; ... ;
    [#x88                       ; callsub ;
    (>> (introduced 4 "callsub")        ;
    (>>= read-int16                     ;
    (λ (dst)                            ;
    (>>= get-pc                         ;
    (λ (pc)                             ;
    (>> (callpush pc)                   ;
    (goto dst)))))))]                   ;
    [#x89                       ; retsub ;
    (>> (introduced 4 "retsub")         ;
    (>>= callpop goto))]                ;
    [#x90                       ; shl   ;
    (>> (introduced 4 "shl")            ;
    (>> (>>= (pop 2)                    ;
    (λ (B A)                            ;
    (push (remainder (* A (expt 2 B))   ;
    (expt 2 64)))))                     ;
    next))]                             ;
                                        ; ... ;
    [#xa0                       ; b+    ;
    (bytesop "b+" bytes+ 10)]           ;
    [#xa1                       ; b-    ;
    (bytesop "b-" bytes- 10)]           ;
    [#xa2                       ; b/    ;
    (bytesop "b/" bytes/ 20)]           ;
    [#xa3                       ; b*    ;
    (bytesop "b*" bytes* 20)]           ;
    [#xa4                       ; b<    ;
    (bytesop "b<" bytes< 1)]            ;
    [#xa5                       ; b>    ;
    (bytesop "b>" bytes> 1)]            ;
    [#xa6                       ; b<=   ;
    (bytesop "b<=" bytes<= 1)]          ;
    [#xa7                       ; b>=   ;
    (bytesop "b>=" bytes>= 1)]          ;
    [#xa8                       ; b==   ;
    (bytesop "b==" bytes= 1)]           ;
    [#xa9                       ; b!=   ;
    (bytesop "b!=" bytes!= 1)]          ;
    [#xaa                       ; b%    ;
    (bytesop "b%" bytes% 20)]           ;
    [#xab                       ; b|    ;
    (bytesop "b|" bytesor 6)]           ;
    [#xac                       ; b&    ;
    (bytesop "b&" bytes& 6)]            ;
    [#xad                       ; b^    ;
    (bytesop "b&" bytes^ 6)]            ;
    [#xae                       ; b~    ;
    (>> (introduced 4 "b~")             ;
    (cost 4)                            ;
    (>> (>>= (pop 1)                    ;
    (λ (X) (push (bytes~ X))))          ;
    next))]                             ;
    [#xaf                       ; bzero ;
    (>> (introduced 4 "bzero")          ;
    (>> (>>= (pop 1)                    ;
    (λ (X) (push (bytes X 0))))         ;
    next))]                             ;
    [(range #xb0 #xff)                  ;
    unused]                             ;
    |#))


(define (op-bytes bc)
  (define (bytesop name op exec-cost)
    (>> (introduced 4 name)
        (cost exec-cost)
        (>>= (pop 2) (λ (B A) (push (op A B))))))
  (match bc))
|#
