#lang racket/base
(require racket/match
         racket/set
         (only-in racket/list append-map)
         "monad.rkt"
         "read-byte.rkt"
         "vm.rkt")

(define (inject lsv bs)
  (hasheq 'logic-sig-version lsv
          'bytecode bs
          'pc 0
          'stack (list)
          'intcblock (list)
          'bytecblock (list)
          'scratch-space (hasheqv)
          'path-condition (hasheqv) ; address → yes/no/both/neither?
          'app-local (list)
          'app-global (list)
          'heap (hash)))

(struct underway (xs state) #:transparent)
(struct returned (code) #:transparent)
(struct failure! (msg) #:transparent)

(define Abstract-Monad
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

(define Abstract-MonadPlus
  (MonadPlus [Monad Abstract-Monad]
             [mzero (λ (st) (list))]
             [mplus (λ (m₀ m₁) (λ (st) (append (m₀ st) (m₁ st))))]))

; field-get : key -> Abstract a
(define-syntax field-get
  (syntax-rules ()
    [(_ key)
     (λ (st) (list (underway (list (hash-ref st 'key)) st)))]
    [(_ key default)
     (λ (st) (list (underway (list (hash-ref st 'key default)) st)))]))
; field-set : key val -> Abstract ()
(define-syntax-rule (field-set key val)
  (λ (st) (list (underway (list) (hash-set st 'key val)))))
; field-update : key f -> Abstract ()
(define-syntax field-update
  (syntax-rules ()
    [(_ key f)
     (λ (st) (list (underway (list) (hash-update st 'key f))))]
    [(_ key f iv)
     (λ (st) (list (underway (list) (hash-update st 'key f iv))))]))


(define Abstract-ReadByte
  (match-let ([(Monad unit >>= >>) Abstract-Monad])
    (ReadByte [Monad Abstract-Monad]
              [read-byte
               (>>= (field-get bytecode)
                    (λ (bc)
                      (>>= (field-get pc)
                           (λ (pc)
                             (if (>= pc (bytes-length bc))
                               (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                               (>> (field-update pc add1)
                                   (unit (bytes-ref bc pc))))))))])))


(require racket/pretty)

(define Abstract-VM
  (match-let ([(MonadPlus [Monad (Monad unit >>= >>)] mzero mplus) Abstract-MonadPlus])
    (define (push x)
      (>>= (field-get pc)
           (λ (pc)
             (>>= (field-get stack)
                  (λ (stk)
                    ; address is pc and length of stack (which is stack slot)
                    ; put pc on stack to recover that address
                    (let ([α (cons pc (length stk))])
                      (>> (field-update heap (λ (σ) (hash-update σ
                                                                 α
                                                                 (λ (xs) (set-add xs x))
                                                                 (set))))
                          (>> (field-update path-condition (λ (pc) (hash-remove pc α)))
                              (field-set stack (cons pc stk))))))))))
    (define pop
      (>>= (field-get stack)
           (match-lambda
             [(cons pc stk)
              (>> (field-set stack stk)
                  (unit (cons pc (length stk))))])))
    (define (assume c)
      (pretty-print c)
      (>>= (field-get heap)
           (λ (σ)
             (pretty-print (hash-ref σ c (set)))
             (raise 11))))
    (define (reject c)
      (raise '10))
    (define (sif cnd thn els)
      ; cnd is an address
      (mplus (>> (assume cnd)
                 thn)
             (>> (reject cnd)
                 els)))
    (define (group-transaction gi fi)
      (>>= (match fi
             [0                         ; Sender
              (unit 'Sender)]
             [1                         ; Fee
              (unit 'Fee)]
             [6                         ; Lease
              (unit 'Lease)]
             [7                         ; Receiver
              (unit 'Receiver)]
             [8                         ; Amount
              (unit 'Amount)]
             [9                         ; CloseRemainderTo
              (unit 'CloseRemainderTo)]
             [16                        ; TypeEnum
              (unit 'TypeEnum)]
             [17                        ; XferAsset
              (unit 'XferAsset)]
             [18                        ; AssetAmount
              (unit 'AssetAmount)]
             [19                        ; AssetSender
              (unit 'AssetSender)]
             [20                        ; AssetReceiver
              (unit 'AssetReceiver)]
             [21                        ; AssetCloseTo
              (unit 'AssetCloseTo)]
             [22                        ; GroupIndex
              (unit 'GroupIndex)]
             [24                        ; ApplicationID
              (>> ((logic-sig-version>= Abstract-VM) 2 "ApplicationID")
                  (unit 'ApplicationID))]
             [25                        ; OnCompletion
              (unit 'OnCompletion)]
             [27                        ; NumAppArgs
              (>> ((logic-sig-version>= Abstract-VM) 2 "NumAppArgs")
                  (unit 'NumAppArgs))]
             [29                        ; NumAccounts
              (>> ((logic-sig-version>= Abstract-VM) 2 "NumAccounts")
                  (unit 'NumAccounts))]
             [32                        ; NumAppArgs
              (>> ((logic-sig-version>= Abstract-VM) 2 "NumAppArgs")
                  (unit 'RekeyTo))]
             [38                        ; ConfigAssetName
              (>> ((logic-sig-version>= Abstract-VM) 2 "ConfigAssetName")
                  (unit 'ConfigAssetName))])
           (λ (f)
             (if (eq? gi 'this-group-index)
               (unit f)
               (unit `(txn ,gi ,f))))))
    (define (group-transaction-array gi fi ai)
      (>>= (match fi
             [26 ; ApplicationArgs
              (>> ((logic-sig-version>= Abstract-VM) 2 "ApplicationArgs")
                  (unit 'ApplicationArgs))]
             [28 ; Accounts
              (>> ((logic-sig-version>= Abstract-VM) 2 "Accounts")
                  (unit 'Accounts))])
           (λ (f)
             (if (eq? gi 'this-group-index)
               (unit `(,f ,ai))
               (unit `(txn ,gi ,f ,ai))))))
    (VM [MonadPlus Abstract-MonadPlus]
        [ReadByte Abstract-ReadByte]
        panic
        [return! return]
        [logic-sig-version (field-get logic-sig-version)]
        [get-pc (field-get pc)]
        [set-pc (λ (pc) (field-set pc pc))]
        [get-bytecode (field-get bytecode)]
        [get-intcblock (field-get intcblock)]
        [put-intcblock (λ (intcs) (field-set intcblock intcs))]
        [get-bytecblock (field-get bytecblock)]
        [put-bytecblock (λ (bytecs) (field-set bytecblock bytecs))]
        push
        pop
        [btoi (λ (x) (unit `(btoi ,x)))]
        [is-zero (λ (c) (sif c (unit #f) (unit #t)))]
        [&& (λ (x y) (unit `(∧ ,x ,y)))]
        [\|\| (λ (x y) (unit `(∨ ,x ,y)))]
        [= (λ (x y) (unit `(= ,x ,y)))]
        [transaction (λ (fi) (group-transaction 'this-group-index fi))]
        group-transaction
        [transaction-array (λ (fi ai) (group-transaction-array 'this-group-index fi ai))]
        group-transaction-array
        [check-final
         (>>= (field-get bytecode)
              (λ (bc)
                (>>= (field-get pc)
                     (λ (pc)
                       (if (= pc (bytes-length bc))
                         (>>= (field-get stack)
                              (match-lambda
                                [(list) (panic "stack is empty at end of program")]
                                [(list x) (return x)]
                                [_ (panic "stack has more than one value at end of program")]))
                         (unit))))))]
        )))

(module+ main
  (require racket/set
           racket/file
           racket/pretty
           "prefix.rkt")
  
  (match (current-command-line-arguments)
    [(vector filename)
     (displayln filename)
     (match ((read-prefix read-ReadByte) (file->bytes filename))
       [(cons lsv bs)
        (let loop ([st (inject lsv bs)]
                   [seen (set)])
          (foldl
           (λ (r seen)
             (if (set-member? seen r)
               seen
               (let ([seen (set-add seen r)])
                 (match r
                   [(underway (list) st)
                    (pretty-print (hash-remove st 'bytecode))
                    (loop st seen)]
                   [(returned code)
                    (cond
                      [(exact-nonnegative-integer? code)
                       (unless (zero? code)
                         (pretty-print st))
                       seen]
                      [else
                       (raise code)])]))))
           seen
           ((step Abstract-VM) st)))])]))
