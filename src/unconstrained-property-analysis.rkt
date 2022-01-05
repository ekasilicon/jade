#lang racket/base
(require (only-in racket/match match match-lambda failure-cont)
         (only-in racket/list append-map)
         racket/set
         "static/object.rkt"
         "static/record.rkt"
         "static/sumtype.rkt"
         "monad.rkt"
         "prefix.rkt"
         "vm.rkt"
         "instruction/read.rkt"
         "instruction/version.rkt")

(define-sumtype Result
  (underway values ς)
  (failure! message)
  (returned code))

; get : key -> UPA a
(define-syntax get
  (syntax-rules ()
    [(_ key)
     (λ (ς) (list (underway [values (list (hash-ref ς 'key))] ς)))]
    [(_ key default)
     (λ (ς) (list (underway [values (list (hash-ref ς 'key default))] ς)))]))
; set : key a -> UPA ()
(define-syntax-rule (put key val)
  (λ (ς) (list (underway [values (list)] [ς (hash-set ς 'key val)]))))
; update : key (a a -> a) [a] -> UPA ()
(define-syntax update
  (syntax-rules ()
    [(_ key f)
     (λ (ς) (list (underway [values (list)] [ς (hash-update ς 'key f)])))]
    [(_ key f iv)
     (λ (ς) (list (underway [values (list)] [ς (hash-update ς 'key f iv)])))]))

(define-syntax p
  (syntax-rules ()
    [(_ unit who n)
     (λ xs (apply unit (build-list n (λ (i) (list* 'who i xs)))))]
    [(_ unit who)
     (p who 1)]))

(define (make-vm lsv)
  (mix (vm/version lsv)
       (inc (unit >>= >> mplus mzero)
            [sha256 (p unit sha256 1)]
            [panic
             (λ (template . args)
               (λ (ς) (list (failure! [message (apply format template args)]))))]
            [return
             (λ (code)
               (if0 code
                    (λ (ς) (list (returned [code 0])))
                    (λ (ς) (list (returned [code 1])))))]
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
                             (unit))))))]
            [constant
             unit]
            [push
             (λ (x) (update stack (λ (stk) (cons x stk))))]
            [pop
             (>>= (get stack)
                  (match-lambda
                    [(cons x stk)
                     (>> (put stack stk)
                         (unit x))]
                    [(list)
                     (panic "tried to pop an empty stack")]))]
            [get-pc
             (get pc)]
            [set-pc
             (λ (pc) (put pc pc))]
            [get-bytecode
             (get bytecode)]
            [get-intcblock
             (get intcblock)]
            [put-intcblock
             (λ (xs) (put intcblock xs))]
            [get-bytecblock
             (get bytecblock)]
            [put-bytecblock
             (λ (bss) (put bytecblock bss))]
            [if0
             (λ (x m₀ m₁)
               (mplus (>> (refute x) m₀)
                      (>> (assume x) m₁)))]               
            [assume
             (match-lambda
               [`(! 0 ,c) (refute c)]
               [`(&& 0 ,c₀ ,c₁)
                (>> (assume c₀)
                    (assume c₁))]
               [`(\|\| 0 ,c₀ ,c₁)
                (mplus (assume c₀)
                       (assume c₁))]
               [`(== 0 ,c₀ ,c₁)
                (cond
                  [(and (exact-nonnegative-integer? c₀)
                        (exact-nonnegative-integer? c₁))
                   (if (= c₀ c₁) (unit) mzero)]
                  [(and (bytes? c₀)
                        (bytes? c₁))
                   (if (bytes=? c₀ c₁) (unit) mzero)]
                  [else
                   (failure-cont)])]
               [c
                (cond
                  [(exact-nonnegative-integer? c)
                   (if (zero? c) mzero (unit))]
                  #;
                  [(ormap (λ (interp) (interp c)) (map car interps)) => values]
                  [else
                   #;(pretty-print c)
                   (unit)])])]
            [refute
             (match-lambda
               [`(! 0 ,c) (assume c)]
               [`(&& 0 ,c₀ ,c₁)
                (mplus (refute c₀)
                       (refute c₁))]
               [`(\|\| 0 ,c₀ ,c₁)
                (>> (refute c₀)
                    (refute c₁))]
               [`(== 0 ,c₀ ,c₁)
                (cond
                  [(and (exact-nonnegative-integer? c₀)
                        (exact-nonnegative-integer? c₁))
                   (if (= c₀ c₁) mzero (unit))]
                  [(and (bytes? c₀)
                        (bytes? c₁))
                   (if (bytes=? c₀ c₁) mzero (unit))]
                  [else
                   (failure-cont)])]
               [c
                (cond
                  [(exact-nonnegative-integer? c)
                   (if (zero? c) (unit) mzero)]
                  #;
                  [(ormap (λ (interp) (interp c)) (map cdr interps)) => values]
                  [else
                   #;(pretty-print c)
                   (unit)])])]
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
            [sha512-256 (p unit sha512-256 1)]
            [u== (p unit == 1)]
            [u<  (p unit < 1)]
            [u+ (p unit + 1)]
            [u- (p unit - 1)]
            [u* (p unit * 1)]
            [u/ (p unit / 1)]
            [u& (p unit & 1)]
            [u% (p unit % 1)]
            [is-zero
             (λ (x) (if0 x (unit #t) (unit #f)))]
            [in-mode
             (λ (target-mode info)
               (>>= (get mode #f)
                    (match-lambda
                      [#f
                       (put mode target-mode)]
                      [mode
                       (if (eq? mode target-mode)
                         (unit)
                         (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))]
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
             (λ (key val) (update app-global (λ (ag) (cons `(put ,key ,val) ag)) (list)))]
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
            [asset-holding-get (p unit asset-holding-get 1)]
            [load
             (λ (i)
               (>>= (get scratch-space (hasheqv))
                    (λ (ss)
                      (cond
                        [(hash-ref ss i #f) => unit]
                        [else
                         (>> #;(log "Scratch space slot ~a uninitialized. The Go VM implementation produces 0." i)
                             (unit 0))]))))]
            [store (λ (i x) (update scratch-space (λ (ss) (hash-set ss i x)) (hasheqv)))]
            [! (p unit ! 1)]
            [&& (p unit && 1)]
            [\|\| (p unit \|\| 1)])
       (instruction-read/version lsv)
       (instruction-version/version lsv)
       (inc ()
            [instruction-name
             (λ (instr) "<instruction name>")])
       (inc (panic
             unit >>= >>)
            [read-byte
             (>>= (get bytecode)
                  (λ (bc)
                    (>>= (get pc)
                         (λ (pc)
                           (if (>= pc (bytes-length bc))
                             (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                             (>> (update pc add1)
                                 (unit (bytes-ref bc pc))))))))])
       (inc (unit)
            [logic-sig-version
             (get LogicSigVersion)])
       monad+-extras
       (inc ()
            [mplus
             (λ ms (λ (ς) (apply append (map (λ (m) (m ς)) ms))))])
       monad-extras
       (inc ()
            [unit (λ values (λ (ς) (list (underway values ς))))]
            [>>= (λ (m f)
                   (λ (ς)
                     (append-map
                      (sumtype-case-lambda Result
                        [(underway [values xs] ς)
                         ((apply f xs) ς)]
                        #:otherwise list)
                      (m ς))))])))

(require racket/pretty)

(define (analyze vm ς)
  (let ([→ (vm 'step)])
    (let loop ([ς ς])
      (foldl
       (λ (r fs)
         (match r
           [(underway [values (list)] ς)
            (loop ς)]
           [(failure! message)
            (printf "failure: ~a\n" message)
            fs]
           [(returned code)
            (displayln code)
            (pretty-print ς)
            (match code
              [1
               (set-add fs ς)]
              [0
               fs])
            #;(displayln "success")
            #;(pretty-print code)
            #;(pretty-print (hash-ref ς 'OnCompletion))
            #;(pretty-print (hash-ref ς 'RekeyTo))
            #;
            (pretty-print (foldl (λ (id ς) (hash-remove ς id)) ς '(ApprovalProgram ClearStateProgram bytecode)))
            #;
            fs]))
       (set)
       (→ ς))))
  )

#;
(require (prefix-in d: "disassemble.rkt"))

(record execution-context (approval-program
                           clear-state-program
                           global-num-byte-slice
                           global-num-uint
                           local-num-byte-slice
                           local-num-uint
                           global-state))

(define (run ctx mapped-constants)
  #;
  (displayln "Disassembly:")
  #;
  (displayln (disassemble-bytes approval-program mapped-constants))
  (match ctx
    [(execution-context approval-program
                        clear-state-program
                           global-num-byte-slice
                           global-num-uint
                           local-num-byte-slice
                           local-num-uint
                           global-state)
     (let ([ς (hasheq 'ApprovalProgram    approval-program
                      'ClearStateProgram  clear-state-program
                      'GlobalNumByteSlice global-num-byte-slice
                      'GlobalNumUint      global-num-uint
                      'LocalNumByteSlice  local-num-byte-slice
                      'LocalNumUint       local-num-uint
                      'GlobalState        global-state
                      'MappedConstants    mapped-constants)])
       (match (((fix prefix-read-byte) 'read-varuint) approval-program) 
         [(cons lsv bytecode)
          (if (<= lsv 3)
            (time
             (analyze (fix (make-vm lsv))
                      (let* ([ς (hash-set ς 'LogicSigVersion lsv)]
                             [ς (hash-set ς 'OnCompletion    (seteq 0 1 2 4 5))]
                             [ς (hash-set ς 'RekeyTo         #f)]
                             [ς (hash-set ς 'bytecode        bytecode)]
                             [ς (hash-set ς 'pc              0)]
                             [ς (hash-set ς 'stack           (list))]
                             [ς (hash-set ς 'scratch-space   (hasheqv))]
                             [ς (hash-set ς 'intcblock       (list))]
                             [ς (hash-set ς 'bytecblock      (list))])
                        ς)))
            (error 'standard "does not support LogicSigVersion = ~a > 3" lsv))]
         [#f
          (error 'standard "unable to read initial logic signature version")]))]))

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
     (exit 255)]))

(provide analyze/raw-binary
         analyze/json-package)

(module+ main
  (require json
           net/base64)

  (require racket/port
           #;
           "disassemble.rkt")
  
  (match (current-command-line-arguments)
    [(vector filenames ...)
     (for-each
      (λ (filename)
        (displayln filename)
        (with-handlers (#;[exn:fail? (λ (e) (displayln (exn-message e)))]
                        )
          (pretty-print (analyze/json-package (call-with-input-file filename port->bytes) (hash)))))
      filenames)]))


#;
(define interps
      (list (letrec ([assume (match-lambda
                               [`(== ,t₀ ,t₁)
                                (cond
                                  [(and (bytes? t₀)
                                        (bytes? t₁))
                                   (if (bytes=? t₀ t₁)
                                     (unit)
                                     fail)]
                                  [(and (exact-nonnegative-integer? t₀)
                                        (exact-nonnegative-integer? t₁))
                                   (if (= t₀ t₁)
                                     (unit)
                                     fail)]
                                  [(equal? t₀ t₁)
                                   (unit)]
                                  [else
                                   (letrec ([present? (match-lambda
                                                        [(i:OnCompletion)
                                                         #t]
                                                        [(i:RekeyTo)
                                                         #t]
                                                        [`(,_ . ,ts)
                                                         (ormap present? ts)]
                                                        [_
                                                         #f])])
                                     (if (or (present? t₀)
                                             (present? t₁))
                                       (>>= (get OnCompletion)
                                            (match-lambda))
                                       (unit)))])]
                               [_ #f])]
                     [refute (match-lambda
                               [`(== ,t₀ ,t₁)
                                (cond
                                  [(and (bytes? t₀)
                                        (bytes? t₁))
                                   (if (bytes=? t₀ t₁)
                                     fail
                                     (unit))]
                                  [(and (exact-nonnegative-integer? t₀)
                                        (exact-nonnegative-integer? t₁))
                                   (if (= t₀ t₁)
                                     fail
                                     (unit))]
                                  [(equal? t₀ t₁)
                                   fail]
                                  [else
                                   (letrec ([present? (match-lambda
                                                        [(i:OnCompletion)
                                                         #t]
                                                        [(i:RekeyTo)
                                                         #t]
                                                        [`(,_ . ,ts)
                                                         (ormap present? ts)]
                                                        [_
                                                         #f])])
                                     (if (or (present? t₀)
                                             (present? t₁))
                                       (match* (t₀ t₁))
                                       (unit)))])]
                               [_ #f])])
              (cons assume refute))))
#;
(define standard-VM
  (match-let ([(Monad+ [monad (Monad unit >>=)] [mplus each]) standard-Monad+]
              [>> (>> standard-Monad)]
              [fail (mzero standard-Monad+)])
    (define <rw
      (match-lambda
        [`(<= ,t₀ ,t₁)
         `(¬ ,(<rw `(> ,t₀ ,t₁)))]
        [`(> ,t₀ ,t₁)
         `(< ,t₁ ,t₀)]))
    (define assume
      (match-lambda
        [`(¬ ,c) (refute c)]
        [`(∧ ,c₀ ,c₁)
         (>> (assume c₀)
             (assume c₁))]
        [`(∨ ,c₀ ,c₁)
         (each (assume c₀)
               (assume c₁))]
        [c
         (cond
           [(ormap (λ (interp) (interp c)) (map car interps)) => values]
           [else
            (pretty-print c)
            (unit)])]))
    (define refute
      (match-lambda
        [`(¬ ,c) (assume c)]
        [`(∧ ,c₀ ,c₁)
         (each (refute c₀)
               (refute c₁))]
        [`(∨ ,c₀ ,c₁)
         (>> (refute c₀)
             (refute c₁))]
        [c
         (cond
           [(ormap (λ (interp) (interp c)) (map cdr interps)) => values]
           [else
            (pretty-print c)
            (unit)])]))
    (VM
     [monad+ standard-Monad+]
     [read-byte]
     [logic-sig-version
      (LogicSigVersion
       [monad standard-Monad]
       [logic-sig-version (get LogicSigVersion)])]
     [in-mode ]
    [getbyte (p getbyte 1)]
    [setbyte (p setbyte 1)]
    [extract (p extract 1)]
    [extract-uint (p extract-uint 1)]
     [&& (λ (s₀ s₁) (unit `(∧ ,s₀ ,s₁)))]
     [\|\| (λ (s₀ s₁) (unit `(∨ ,s₀ ,s₁)))]
     [uint-alu
      (unimplemented
       ArithmeticLogicUnit
       [== (λ (a b) (unit `(== ,a ,b)))]
       [+ (λ (a b) (unit `(+ ,a ,b)))]
       [-
        (λ (x y)
           (symbolic-if (<rw `(<= ,x ,y))
                        (unit `(- ,x ,y))
                        (panic "-: ~a > ~a" y x)))]
       [* (λ (a b) (unit `(* ,a ,b)))]
       [/ (λ (a b) (unit `(/ ,a ,b)))]
       [& (λ (a b) (unit `(& ,a ,b)))]
       [% (λ (a b) (unit `(% ,a ,b)))]
       [< (λ (a b) (unit `(< ,a ,b)))])]
     [bzero
      (p bzero 1)]
     [byte-alu
      (unimplemented ArithmeticLogicUnit)]
     [internal-transaction
      (unimplemented InternalTransaction)]
     panic
     [return! return]
     [push
      (λ (x) (update stack (λ (stk) (cons x stk))))]
     [pop
      (>>= (get stack)
           (match-lambda
             [(cons x stk)
              (>> (put stack stk)
                  (unit x))]
             [(list)
              (panic "tried to pop an empty stack")]))]
     [is-zero (λ (x) (symbolic-if x (unit #f) (unit #t)))]
     [shl (p shl 1)]
     [shr (p shr 1)]
     [bitlen (p bitlen 1)]
     [len (λ (x) (unit `(len ,x)))]
     [itob (λ (x) (unit `(itob ,x)))]
     [btoi
      (λ (x)
        (symbolic-if (<rw `(<= (len ,x) 8))
                     (unit `(btoi ,x))
                     (panic "btoi: ~a longer than 8 bytes" x)))]
     [concat (λ (x y)
               (symbolic-if (<rw `(<= (len (concat ,x ,y)) 4096))
                            (unit `(concat ,x ,y))
                            (panic "concat: ~a and ~a longer than 4096 bytes" x y)))]
     [substring
         (λ (a s e)
           (symbolic-if (<rw `(∧ (<= ,s ,e)
                                 (<= ,e (len ,a))))
                        (unit `(substring ,a ,s ,e))
                        (panic "substring: out of bounds ~v ~v ~v" a s e)))]
     [transaction
      (λ (f)
        (enumtype-case i:TransactionField f
          [(i:Sender i:OnCompletion i:TypeEnum i:GroupIndex)
           (unit f)]
          [(i:ApplicationID i:NumAppArgs i:NumAccounts i:RekeyTo)
           (>> ((logic-sig-version>= standard-VM) 2 (i:transaction-field-name f))
               (unit f))]
          #:otherwise
          (λ (f) (error 'transaction "handle case for ~a" f))))]
     [group-transaction
      (λ (gi f)
        (unit `(txn-property ,gi ,f)))]
     group-transaction-array
     [transaction-array
      (λ (f ai) (group-transaction-array (i:GroupIndex) f ai))]
     [global
      (λ (f)
        (enumtype-case i:GlobalField f
          [(i:MinTxnFee i:MinBalance i:MaxTxnLife i:ZeroAddress i:GroupSize)
           (unit f)]
          [(i:LogicSigVersion)
           (>> ((logic-sig-version>= standard-VM) 2 (i:global-field-name f))
               (get LogicSigVersion))]
          [(i:Round i:LatestTimestamp i:CurrentApplicationID)
           (>> ((logic-sig-version>= standard-VM) 2 (i:global-field-name f))
               (unit f))]
          [(i:CreatorAddress)
           (>> ((logic-sig-version>= standard-VM) 3 (i:global-field-name f))
               (unit f))]
          [(i:CurrentApplicationAddress i:GroupID)
           (>> ((logic-sig-version>= standard-VM) 5 (i:global-field-name f))
               (unit f))]
          #:otherwise
          (λ (f) (error 'global "handle case for ~a" f))))]
     [balance
      (p balance 1)
      #;
      (λ (acct)
        (>> (assume (<rw `(<= (+ ,(i:NumApplications)
                                 (+ ,(i:NumAccounts)
                                    ,(i:NumAssets)))
                              8)))
            (symbolic-if (<rw `(<= ,acct ,(i:NumAccounts)))
                         )))]
     [min-balance (p min-balance 1)]
     
     [app-global-get-ex
      (λ (app key)
        (>>= (get app-global (list))
             (letrec ([lookup (match-lambda
                                [(list)
                                 (unit `(app-global ,app ,key)
                                       `(app-global-exists ,app ,key))]
                                [(cons `(put ,put-key ,val)
                                       al)
                                 (symbolic-if `(== ,key ,put-key)
                                              (unit val 1)
                                              (lookup al))])])
               lookup)))]
     [app-global-del
      (p app-global-del 0)]
     [app-local-del
      (p app-local-del 0)]
     
     [asset-params-get
      (p asset-params-get 2)]
     [app-params-get
      (p app-params-get 2)]
     
     [group-aid (p group-aid 1)]
     [group-load (p group-load 1)]
     [getbit (p getbit 1)]
     [setbit (p setbit 1)]
     [check-final
      ])))


#|
(require racket/match
         racket/set
         (only-in racket/list append-map)
         (only-in racket/string string-join)
         "static/sumtype.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "prefix.rkt"
         "vm.rkt"
         (prefix-in i: "instruction.rkt"))


(define (inject bytecode OnCompletion RekeyTo sk fk)
  (match ((read-varuint prefix-ReadByte) bytecode)
    [(cons LogicSigVersion bytecode)
     (if (> LogicSigVersion 3)
       (fk "LogicSigVersion = ~a > 3" LogicSigVersion)
       (sk (hasheq 'LogicSigVersion LogicSigVersion
                   'OnCompletion    OnCompletion
                   'RekeyTo         RekeyTo
                   'bytecode        bytecode
                   'pc              0
                   'stack           (list)
                   'scratch-space   (hasheqv)
                   'global-storage  (list)
                   'local-storage   (list)
                   'intcblock       (list)
                   'bytecblock      (list))))]))

; get : key -> UPA a
(define-syntax get
  (syntax-rules ()
    [(_ key)
     (λ (ς) (list (underway [values (list (hash-ref ς 'key))] ς)))]
    [(_ key default)
     (λ (ς) (list (underway [values (list (hash-ref ς 'key default))] ς)))]))
; set : key val -> UPA ()
(define-syntax-rule (put key val)
  (λ (ς) (list (underway [values (list)] [ς (hash-set ς 'key val)]))))
; update : key f -> UPA ()
(define-syntax update
  (syntax-rules ()
    [(_ key f)
     (λ (ς) (list (underway [values (list)] [ς (hash-update ς 'key f)])))]
    [(_ key f iv)
     (λ (ς) (list (underway [values (list)] [ς (hash-update ς 'key f iv)])))]))


(define-sumtype Result
  (underway values ς)
  (returned code constraints)
  (failure! message))

(define ((unit . xs) ς) (list (underway [values xs] ς)))
(define ((return code) ς) (list (returned code
                                          [constraints (hasheq 'OnCompletion    (hash-ref ς 'OnCompletion)
                                                               'RekeyTo         (hash-ref ς 'RekeyTo)
                                                               'ApplicationArgs (hash-ref ς 'ApplicationArgs (hasheqv))
                                                               'tbh-assumptions (hash-ref ς 'tbh-assumptions (set))
                                                               'tbh-refutations (hash-ref ς 'tbh-refutations (set))
                                                               'assumptions     (hash-ref ς 'assumptions (set))
                                                               'refutations     (hash-ref ς 'refutations (set)))])))
(define ((panic template . args) ς) (list (failure! [message (apply format template args)])))

(define ((>>= m f) ς)
  (append-map
   (sumtype-case-lambda Result
     [(underway [values xs] ς)
      ((apply f xs) ς)]
     #:otherwise list)
   (m ς)))

(define monad (Monad unit >>=))

(define ((mplus . ms) ς) (append-map (λ (m) (m ς)) ms))

(define monad+ (Monad+ monad mplus))

(define read-byte
  (let ([>> (>> monad)])
    (ReadByte monad
              [read-byte
               (>>= (get bytecode)
                    (λ (bc)
                      (>>= (get pc)
                           (λ (pc)
                             (if (>= pc (bytes-length bc))
                               (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                               (>> (update pc add1)
                                   (unit (bytes-ref bc pc))))))))])))

(define logic-sig-version
  (LogicSigVersion monad
                   [logic-sig-version
                    (get LogicSigVersion)]))

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax unimplemented
  (syntax-parser
    [(_ feature:string)
     #'(λ (ς) (error 'unimplemented "~a" feature))]
    [(_ feature:string arity:exact-nonnegative-integer)
     (with-syntax ([(x ...) (generate-temporaries (build-list (syntax->datum #'arity) values))])
       #'(λ (x ...) (λ (ς) (error 'unimplemented "~a; arguments were ~v" feature (list x ...)))))]))

(define-syntax s
  (syntax-parser
    [(_ arity:exact-nonnegative-integer tag:id ...)
     (with-syntax ([(x ...) (generate-temporaries (build-list (syntax->datum #'arity) values))])
       #'(λ (x ...) (unit (list 'tag x ...) ...) ))]))

(define (machine-log template . args) (unit))

(define vm
  (let ([>> (>> monad)]
        [each mplus]
        [fail (mzero monad+)]
        [uint-alu (ArithmeticLogicUnit
                   [+ (λ (x y)
                        (if (and (exact-nonnegative-integer? x)
                                 (exact-nonnegative-integer? y))
                          (unit (+ x y))
                          (unit `(+ ,x ,y))))
                      #;(s 2 +)
                      ]
                   [- (s 2 -)]
                   [/ (s 2 /)]
                   [* (s 2 *)]
                   [% (s 2 %)]
                   [& (s 2 &)]
                   [\| (s 2 \|)]
                   [^ (s 2 ^)]
                   [~ (s 1 ~)]
                   [< (s 2 <)]
                   [== (s 2 ==)])]
        [bytes-alu (ArithmeticLogicUnit
                    [+ (s 2 b+)]
                    [- (s 2 b-)]
                    [/ (s 2 b/)]
                    [* (s 2 b*)]
                    [% (s 2 b%)]
                    [& (s 2 b&)]
                    [\| (s 2 b\|)]
                    [^ (s 2 b^)]
                    [~ (s 1 b~)]
                    [< (s 2 b<)]
                    [== (s 2 b==)])])
    (define (sif c t f)
      (each (>> (assume c) t)
            (>> (refute c) f)))
    (define (contains? ? c)
      (if (? c)
        #t
        (match c
          [(cons tag cs)
           (ormap (λ (c) (contains? ? c)) cs)]
          [_
           #f])))
    (define-syntax-rule (log-assumption property-name c)
      (update tbh-assumptions (λ (cs) (set-add cs c)) (set)))
    (define-syntax-rule (log-refutation property-name c)
      (update tbh-refutations (λ (cs) (set-add cs c)) (set)))
    (define interpretations
      (list (list (sumtype-predicate i:OnCompletion)
                  (match-lambda
                    [(or `(== ,(i:OnCompletion) ,(? exact-nonnegative-integer? n))
                         `(== ,(? exact-nonnegative-integer? n) ,(i:OnCompletion)))
                     (>>= (get OnCompletion)
                          (λ (oc₀)
                            (let ([oc (set-intersect oc₀ (seteqv n))])
                              (if (set-empty? oc)
                                fail
                                (put OnCompletion oc)))))]
                    [c (log-assumption OnCompletion c)])
                  (match-lambda
                    [(or `(== ,(i:OnCompletion) ,(? exact-nonnegative-integer? n))
                         `(== ,(? exact-nonnegative-integer? n) ,(i:OnCompletion)))
                     (>>= (get OnCompletion)
                          (λ (oc₀)
                            (let ([oc (set-intersect oc₀ (set-subtract (seteqv 0 1 2 4 5) (seteqv n)))])
                              (if (set-empty? oc)
                                fail
                                (put OnCompletion oc)))))]
                    [c (log-refutation OnCompletion c)]))
            (list (sumtype-predicate i:RekeyTo)
                  (match-lambda
                    [`(== ,(i:RekeyTo) ,(i:ZeroAddress))
                     (>>= (get RekeyTo)
                          (match-lambda
                            [#f
                             (put RekeyTo `(= ,(i:ZeroAddress)))]
                            [`(= ,(i:ZeroAddress))
                             (unit)]
                            [`(≠ ,(i:ZeroAddress))
                             fail]))]
                    [c (log-assumption RekeyTo c)])
                  (match-lambda
                    [`(== ,(i:RekeyTo) ,(i:ZeroAddress))
                     (>>= (get RekeyTo)
                          (match-lambda
                            [#f
                             (put RekeyTo `(≠ ,(i:ZeroAddress)))]
                            [`(= ,(i:ZeroAddress))
                             fail]
                            [`(≠ ,(i:ZeroAddress))
                             (unit)]))]
                    [c (log-refutation RekeyTo c)]))
            (list (sumtype-predicate i:ApplicationArgs)
                  (match-lambda
                    [(or `(== (transaction-array ,(i:ApplicationArgs) ,(? exact-nonnegative-integer? i)) ,x)
                         `(== ,x (transaction-array ,(i:ApplicationArgs) ,(? exact-nonnegative-integer? i))))
                     (>>= (get ApplicationArgs (hasheqv))
                          (λ (ApplicationArgs)
                            (cond
                              [(hash-ref ApplicationArgs i #f)
                               => (λ (x₀)
                                    (match* (x₀ x)
                                      [(`(= ,z) z) (unit)]
                                      [(`(= ,(? bytes?)) (? bytes?)) fail]
                                      [(`(≠ ,z) z) fail]
                                      [(`(≠ ,_) _) (unit)]))]
                              [else
                               (put ApplicationArgs (hash-set ApplicationArgs i `(= ,x)))])))]
                    [c (log-assumption ApplicationArgs c)])
                  (match-lambda
                    [(or `(== (transaction-array ,(i:ApplicationArgs) ,(? exact-nonnegative-integer? i)) ,x)
                         `(== ,x (transaction-array ,(i:ApplicationArgs) ,(? exact-nonnegative-integer? i))))
                     (>>= (get ApplicationArgs (hasheqv))
                          (λ (ApplicationArgs)
                            (cond
                              [(hash-ref ApplicationArgs i #f)
                               => (λ (x₀)
                                    (match* (x₀ x)
                                      [(`(= ,z) z) fail]
                                      [(`(= ,(? bytes?)) (? bytes?)) fail]
                                      [(`(≠ ,z) z) (unit)]
                                      [(`(≠ ,_) _) (unit)]))]
                              [else
                               (put ApplicationArgs (hash-set ApplicationArgs i `(≠ ,x)))])))]
                    [c (log-refutation ApplicationArgs c)]))))
    (define assume
      (match-lambda
        [`(! ,c)
         (refute c)]
        [`(\|\| ,c₀ ,c₁)
         (each (assume c₀)
               (assume c₁))]
        [`(&& ,c₀ ,c₁)
         (>> (assume c₀)
             (assume c₁))]
        [(? exact-nonnegative-integer? n)
         (if (zero? n) fail (unit))]
        [`(== ,(? exact-nonnegative-integer? x)
              ,(? exact-nonnegative-integer? y))
         (if (= x y) (unit) fail)]
        [`(< ,(? exact-nonnegative-integer? x)
             ,(? exact-nonnegative-integer? y))
         (if (< x y) (unit) fail)]
        [c
         (cond
           [(ormap
             (match-lambda
               [(list ? interpretation _)
                (and (contains? ? c) (interpretation c))])
             interpretations)
            => values]
           [else
            (update assumptions (λ (cs) (set-add cs c)) (set))])]))
    (define refute
      (match-lambda
        [`(! ,c)
         (assume c)]
        [`(\|\| ,c₀ ,c₁)
         (>> (refute c₀)
             (refute c₁))]
        [`(&& ,c₀ ,c₁)
         (each (refute c₀)
               (refute c₁))]
        [(? exact-nonnegative-integer? n)
         (if (zero? n) (unit) fail)]
        [`(== ,(? exact-nonnegative-integer? x)
              ,(? exact-nonnegative-integer? y))
         (if (= x y) fail (unit))]
        [`(< ,(? exact-nonnegative-integer? x)
             ,(? exact-nonnegative-integer? y))
         (if (< x y) fail (unit))]
        [c
         (cond
           [(ormap
             (match-lambda
               [(list ? _ interpretation)
                (and (contains? ? c) (interpretation c))])
             interpretations)
            => values]
           [else
            (update refutations (λ (cs) (set-add cs c)) (set))])]))
    (define (logic-value x)
      (if (exact-nonnegative-integer? x)
        (if (zero? x) 0 1)
        `(! (! ,x))))
    (VM monad+
        read-byte
        logic-sig-version
        panic
        [return (λ (code) (sif code (return 1) (return 0)))]
        [in-mode
         (λ (target-mode info)
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
        [arg (s 1 arg)]
        [args (s 1 args)]
        [push (λ (x) (update stack (λ (stk) (cons x stk))))]
        [pop
         (>>= (get stack)
              (match-lambda
                [(cons x stk)
                 (>> (put stack stk)
                     (unit x))]
                [(list)
                 (panic "tried to pop an empty stack")]))]
        [push-call #;(λ (ret-pc) (update return-points (λ (ret-pcs) (set-add ret-pcs ret-pc)) (seteqv)))
                   (λ (pc) (update call-stack (λ (stk) (cons pc stk)) (list)))]
        [pop-call
         #;
         (>>= (get return-points (seteqv))
              (λ (ret-pcs)
                (let loop ([pcs ret-pcs])
                  (if (set-empty? pcs)
                    fail
                    (let ([pc (set-first pcs)]
                          [pcs (set-rest pcs)])
                      (each (unit pc)
                            (loop pcs)))))))
         (>>= (get call-stack)
              (match-lambda
                [(cons pc stk)
                 (>> (put call-stack stk)
                     (unit pc))]
                [(list)
                 (panic "tried to pop an empty call stack")]))]
        [sha256 (s 1 sha256)]
        [keccak256 (s 1 keccak256)]
        [sha512-256 (s 1 sha512-256)]
        [ed25519verify (unimplemented "ed25519verify" 3)]
        [ecdsa-verify (unimplemented "ecdsa-verify" 6)]
        [ecdsa-pk-decompress (unimplemented "ecdsa-pk-decompress" 2)]
        [ecdsa-pk-recover (unimplemented "ecdsa-pk-recover" 5)]
        uint-alu
        [! (s 1 !)]
        [len (s 1 len)]
        [itob (s 1 itob)]
        [btoi (s 1 btoi)]
        [mulw (s 2 mulw0 mulw1)]
        [addw (s 2 addw0 addw1)]
        [divmodw (s 4 divmodw0 divmodw1 divmodw2 divmodw3)]
        [expw (s 2 expw0 expw1)]
        [shl (s 2 shl)]
        [shr (s 2 shr)]
        [is-zero (λ (c) (sif c (unit #f) (unit #t)))]
        [&& 
         (λ (x y)
           (let ([lx (logic-value x)]
                 [ly (logic-value y)])
             (cond
               [(or (equal? lx 0)
                    (equal? ly 0))
                (unit 0)]
               [(equal? lx 1)
                (unit ly)]
               [(equal? ly 1)
                (unit lx)]
               [else
                (unit `(&& ,x ,y))])))]
        [\|\|
         (λ (x y)
           (let ([lx (logic-value x)]
                 [ly (logic-value y)])
             (cond
               [(or (equal? lx 1)
                    (equal? ly 1))
                (unit 1)]
               [(equal? lx 0)
                (unit ly)]
               [(equal? ly 0)
                (unit lx)]
               [else
                (unit `(\|\| ,x ,y))])))]
        [concat (s 2 concat)]
        [substring (s 3 substring)]
        [getbyte (s 2 getbyte)]
        [setbyte (s 3 setbyte)]
        [getbit (s 2 getbit)]
        [setbit (s 3 setbit)]
        [extract (s 3 extract)]
        [extract-uint (s 3 extract-uint)]
        [bitlen (s 1 bitlen)]
        [bzero (s 1 bzero)]
        bytes-alu
        [global unit
        #;
         (sumtype-case-lambda i:GlobalField
           [(i:GroupSize)
            (let loop ([n 16])
              (if (zero? n)
                fail
                (each (unit n)
                      (loop (sub1 n)))))]
           #:otherwise unit)]
        [transaction unit]
        [group-transaction (s 2 group-transaction)]
        [transaction-array (s 2 transaction-array)]
        [group-transaction-array (s 3 group-transaction-array)]
        [group-aid (unimplemented "group-aid" 1)]
        [group-load (unimplemented "group-load" 2)]
        [load
         (λ (i)
           (>>= (get scratch-space)
                (λ (ss)
                  (cond
                    [(hash-ref ss i #f) => unit]
                    [else
                     (>> (machine-log "Scratch space slot ~a uninitialized. The Go VM implementation produces 0." i)
                         (unit 0))]))))]
        [store (λ (i x) (update scratch-space (λ (ss) (hash-set ss i x))))]
        [balance (s 1 balance)]
        [min-balance (s 1 min-balance)]
        [app-opted-in (s 2 app-opted-in)]
        [app-local-get (s 2 app-local-get)]
        [app-local-put (s 3 app-local-put)]
        [app-local-del (s 2 app-local-del)]
        [app-local-get-ex (s 3 app-local-get-ex)]
        [app-global-get (s 1 app-global-get)]
        [app-global-put (s 2 app-global-put)]
        [app-global-del (s 1 app-global-del)]
        [app-global-get-ex (s 2 app-global-get-ex)]
        [asset-holding-get (s 3 asset-holding-get)]
        [asset-params-get (s 2 asset-params-get)]
        [app-params-get (s 2 app-params-get)]
        [internal-transaction
         (InternalTransaction [begin  (unit)]
                              [next   (unit)]
                              [field  (s 2 itxn-field)]
                              [submit (unit)]
                              [access (s 1 itxn)]
                              [array  (s 2 itxn-array)])]
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
                         (unit))))))])


(define ⇒
  (step vm))

(require (rename-in (only-in racket/base log) [log ln]))
(define (analyze bytecode sk fk)
  (inject bytecode
          (seteqv 0 1 2 4 5) #f
          (λ (ς)
            (let ([capacity (let ([n (bytes-length bytecode)]
                                  [sqr (λ (n) (* n n))])
                              (inexact->exact (floor (* n (sqr (ln n))))))])
              (let loop ([todo (list ς)]
                         [seen (set)]
                         [final (set)])
                (if (< (set-count seen) capacity)
                  (match todo
                    [(list)
                     (sk final)]
                    [(cons ς todo)
                     (if (set-member? seen ς)
                       (loop todo seen final)
                       (let-values ([(todo final) (for/fold ([todo todo]
                                                             [final final])
                                                            ([r (in-list (⇒ ς))])
                                                    (sumtype-case Result r
                                                      [(underway [values (list)] ς)
                                                       (values (cons ς todo) final)]
                                                      [(returned code constraints)
                                                       (unless (exact-nonnegative-integer? code)
                                                         (raise code))
                                                       (values todo (set-add final r))]
                                                      [(failure! message)
                                                       #;(displayln message)
                                                       (values todo final)]))])
                         (loop todo (set-add seen ς) final)))])
                  (fk "exceeded ~a states" capacity)))))
          fk))

#|

first, report on the cases when OnCompletion is not constrained and when RekeyTo is not constrained
report from least to most constrained.
remember to include whether an unknown property exists

next, report on the cases in which an unknown property exists.
don't overlap with the first report, so remove those from the candidate set.

then talk about the other states, if a full report is desired.

don't order by property but by how unconstrained they are, so talk about unconstrained RekeyTo before
reporting on entirely-constrained OnCompletion (or any other property).
but the relative ordering of properties may be immaterial.


|#

(module+ main
  (require racket/pretty
           "../test/algoexplorer/extract.rkt")
  (for ([path (in-list (vector->list (current-command-line-arguments)))])
    (let ([bytecode (file-extract path 'approval-program)])
      #;(displayln path)
      (analyze bytecode
               (λ (finals)
                 (define quantity-description
                   (match-lambda
                     [(? exact-nonnegative-integer? n)
                      (number->string n)]
                     [(? bytes? bs)
                      (bytes->string/utf-8 bs)]
                     [(? (sumtype-predicate i:GlobalField) f)
                      (i:global-field-name f)]
                     [(? (sumtype-predicate i:TransactionField) f)
                      (i:transaction-field-name f)]
                     [`(group-transaction ,(? exact-nonnegative-integer? i) ,f)
                      (format "txns[~a].~a" i (i:transaction-field-name f))]
                     [`(app-global-get ,(? bytes? key))
                      (format "globals['~a']" key)]
                     [`(+ ,x ,y)
                      (string-append (quantity-description x)
                                     " plus "
                                     (quantity-description y))]))
                 (define assumption-description
                   (match-lambda
                     [`(== ,x ,y)
                      (string-append (quantity-description x)
                                     " is "
                                     (quantity-description y))]))
                 (define refutation-description
                   (match-lambda
                     [`(== ,x ,y)
                      (string-append (quantity-description x)
                                     " is not "
                                     (quantity-description y))]))
                 (define (report c ps)
                   (displayln "Possible outcome:\n")
                   (for ([(p is-unconstrained?) (in-hash ps)])
                     (when (is-unconstrained? c)
                       (printf "~a is NOT constrained\n" p)))
                   #;
                   (let ([oc (hash-ref c 'OnCompletion)])
                     (cond
                       [(= (set-count oc) 5)
                        (displayln "is NOT constrained")]
                       [(> (set-count oc) 2)
                        (let ([oc (set-subtract (seteqv 0 1 2 4 5) oc)])
                          (printf "is constrained NOT to be ~a\n"
                                  (match (map number->string (set->list oc))
                                    [(list x) x]
                                    [(list x y) (string-append x " or " y)]
                                    [xs (string-join xs ", " #:before-last ", or ")])))]
                       [else
                        (printf "is constrained to be ~a\n"
                                (match (map number->string (set->list oc))
                                  [(list x) x]
                                  [(list x y) (string-append x " or " y)]
                                  [xs (string-join xs ", " #:before-last ", or ")]))]))
                   #;
                   (match (hash-ref c 'RekeyTo)
                     [#f
                      (displayln "is NOT constrained")]
                     [(i:ZeroAddress)
                      (displayln "is constrained to be the zero address")])
                   (let ([assumptions (hash-ref c 'assumptions)]
                         [refutations (hash-ref c 'refutations)])
                     (unless (and (set-empty? assumptions)
                                  (set-empty? refutations))
                       (displayln "\nwhen\n")
                       (for ([c (in-set assumptions)])
                         (display "  ")
                         (displayln (assumption-description c)))
                       (for ([c (in-set refutations)])
                         (display "  ")
                         (displayln (refutation-description c)))))
                   (let ([assumptions (hash-ref c 'tbh-assumptions)]
                         [refutations (hash-ref c 'tbh-refutations)])
                     (unless (and (set-empty? assumptions)
                                  (set-empty? refutations))
                       (displayln "\nbut it may be that\n")
                       (for ([c (in-set assumptions)])
                         (display "  ")
                         (displayln (assumption-description c)))
                       (for ([c (in-set refutations)])
                         (display "  ")
                         (displayln (refutation-description c)))
                       (displayln "\nwhich may undermine the analysis of this possible outcome"))))
                 (define (find-max cs ps)
                   (for/fold ([max-c #f]
                              [max-c-count 0])
                             ([c (in-set cs)])
                     (let ([c-count (for/sum ([(p is-unconstrained?) (in-hash ps)])
                                    (if (is-unconstrained? c) 1 0))])
                       (if (>= c-count max-c-count)
                         (values c c-count)
                         (values max-c max-c-count)))))
                 (let ([cs (for/fold ([cs (set)])
                                     ([r (in-set finals)])
                             (match-let ([(returned code constraints) r])
                               (if (zero? code)
                                 cs
                                 (set-add cs constraints))))])
                          ; 1. report any properties that are never constrained
                          ;    remove those properties from the rest of the report
                   (let* ([ps (let ([ps (hasheq 'RekeyTo (λ (c) (not (hash-ref c 'RekeyTo #f)))
                                                'OnCompletion (λ (c) (= (set-count (hash-ref c 'OnCompletion (seteq 0 1 2 4 5))) 5)))])
                                (for/fold ([ps ps]) ([(p is-unconstrained?) (in-hash ps)])
                                  (cond
                                    [(for/and ([c (in-set cs)])
                                       (is-unconstrained? c))
                                     (printf "~a is NOT constrained AT ALL in any outcome\n" p)
                                     (hash-remove ps p)]
                                    [else
                                     ps])))]
                          ; 2. report states that have at least one unconstrained property, ordered by the number of such
                          ;    properties they have.
                          ;    report on tbh assumptions/refutations
                          ;    remove such states from the set
                          [cs (let loop ([cs cs])
                                (let-values ([(c c-count) (find-max cs ps)])
                                  (if (zero? c-count)
                                    cs
                                    (begin
                                      (report c ps)
                                      (loop (set-remove cs c))))))]
                          ; 3. report states that have tbh assumptions/refutations
                          ;    remove such states from the set
                          [cs (for/fold ([cs cs])
                                        ([c (in-set cs)])
                                (if (and (set-empty? (hash-ref c 'tbh-assumptions))
                                         (set-empty? (hash-ref c 'tbh-refutations)))
                                  cs
                                  (begin
                                    (displayln "some missing assumptions/refutations")
                                    (set-remove cs c))))])
                     (printf "There ~a ~a remaining discovered possible outcome~a.\n"
                             (if (= (set-count cs) 1) "is" "are")
                             (set-count cs)
                             (if (= (set-count cs) 1) "" "s")))))
               (λ (template . args)
                 (printf #<<MESSAGE
Unable to analyze TEAL program:

  ~a

MESSAGE
                            (apply format template args)))))))
|#
