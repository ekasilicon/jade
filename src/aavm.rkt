#lang racket/base
(require (except-in racket/match ==)
         (only-in racket/list append-map)
         racket/set
         "static/object.rkt"
         "static/sumtype.rkt"
         "monad.rkt"
         "prefix.rkt"
         "vm.rkt"
         (prefix-in i: "instruction.rkt"))

(define-sumtype Result
  (underway values ς)
  (failure! message)
  (returned code))

; get : key -> Standard a
(define-syntax get
  (syntax-rules ()
    [(_ key)
     (λ (ς) (list (underway [values (list (hash-ref ς 'key))] ς)))]
    [(_ key default)
     (λ (ς) (list (underway [values (list (hash-ref ς 'key default))] ς)))]))
; set : key val -> Standard ()
(define-syntax-rule (put key val)
  (λ (ς) (list (underway [values (list)] [ς (hash-set ς 'key val)]))))
; update : key f -> Standard ()
(define-syntax update
  (syntax-rules ()
    [(_ key f)
     (λ (ς) (list (underway [values (list)] [ς (hash-update ς 'key f)])))]
    [(_ key f iv)
     (λ (ς) (list (underway [values (list)] [ς (hash-update ς 'key f iv)])))]))

(define (make-vm lsv)
  (mix (vm/version lsv)
       (inc (unit >>= >> mplus mzero)
            [sha256 (λ (x) (raise x) (unit `(sha256 ,x)))]
            [panic
             (λ (template . args)
               (λ (ς) (list (failure! [message (apply format template args)]))))]
            [return
             (λ (code)
               (if0 code
                 (λ (ς) (list (returned [code 0])))
                 (λ (ς) (list (returned [code 1])))))]
            [check-final
             (unit)]
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
            [transaction unit]
            [global unit]
            [group-transaction (p unit group-transaction 1)]
            [transaction-array (p unit transaction-array 1)]
            [btoi (p unit btoi 1)]
            [itob (p unit itob 1)]
            [u== (p unit == 1)]
            [u<  (p unit < 1)]
            [u+ (p unit + 1)]
            [u* (p unit * 1)]
            [u/ (p unit / 1)]
            [u% (p unit % 1)]
            [if0
             (λ (x m₀ m₁)
               (mplus (>> (refute x) m₀)
                      (>> (assume x) m₁)))]
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
                   (pretty-print c)
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
                   (pretty-print c)
                   (unit)])])]
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
       (i:read/version lsv)
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


(define-syntax p
  (syntax-rules ()
    [(_ unit who n)
     (λ xs (apply unit (build-list n (λ (i) (list* 'who i xs)))))]
    [(_ unit who)
     (p who 1)]))

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
    (define (symbolic-if x c a)
      (each (>> (assume x) c)
            (>> (refute x) a)))
    (define (group-transaction-array gi f ai)
      (unit `(group-transaction-array ,gi ,f ,ai)))
    (VM
     [monad+ standard-Monad+]
     [read-byte
      (ReadByte [monad standard-Monad]
                [read-byte
                 (>>= (get bytecode)
                      (λ (bc)
                        (>>= (get pc)
                             (λ (pc)
                               (if (>= pc (bytes-length bc))
                                 (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                                 (>> (update pc add1)
                                     (unit (bytes-ref bc pc))))))))])]
     [logic-sig-version
      (LogicSigVersion
       [monad standard-Monad]
       [logic-sig-version (get LogicSigVersion)])]
     [in-mode ]
     [arg (p arg 1)]
     [args (p args 1)]
     [push-call (p push-call 0)]
     [pop-call (p pop-call 0)]
     [sha256 (p sha256 1)]
     [keccak256 (p keccak256 1)]
     [sha512-256 (p sha512-256 1)]
     [ed25519verify (p ed25519verify 1)]
     [ecdsa-verify (p ecdsa-verify 1)]
     [ecdsa-pk-decompress (p ecdsa-pk-decompress 2)]
     [ecdsa-pk-recover (p ecdsa-pk-recover 2)]
     #|
     [uint-alu
      (ArithmeticLogicUnit)]
     [! (p ! 1)]
     [len (p len 1)]
     [itob (p itob 1)]
     [btoi (p btoi 1)]
     |#
     [mulw (p mulw 2)]
     [addw (p addw 2)]
     [expw (p expw 2)]
     [divmodw (p divmodw 4)]
     #|
     [&& (p && 1)]
     [\|\| (p \|\| 1)]
    [concat (p concat 1)]
    [substring (p substring 1)]
    |#
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
     [app-opted-in
      (λ (acct app)
        (unit `(app-opted-in ,acct ,app)))]
     
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
     [app-local-get
      (λ (acct key)
        (>>= (get app-local (list))
             (letrec ([lookup (match-lambda
                                [(list)
                                 (unit `(app-local ,acct ,key))]
                                [(cons `(put ,put-acct ,put-key ,val)
                                       al)
                                 (symbolic-if `(∧ (== ,acct ,put-acct)
                                                  (== ,key  ,put-key))
                                              (unit val)
                                              (lookup al))])])
               lookup)))]
     [app-local-get-ex
      (λ (acct app key)
        (>>= (get app-local (list))
             (letrec ([lookup (match-lambda
                                [(list)
                                 (unit `(app-local ,acct ,app ,key)
                                       `(app-local-exists ,acct ,app ,key))]
                                [(cons `(put ,put-acct ,put-key ,val)
                                       al)
                                 (symbolic-if `(∧ (== ,acct ,put-acct)
                                                  (== ,key  ,put-key))
                                              (unit val 1)
                                              (lookup al))])])
               lookup)))]
     [app-local-put
      (λ (acct key val)
        (update app-local (λ (al) (cons `(put ,acct ,key ,val) al)) (list)))]
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


(require racket/pretty)

(define (analyze vm ς)
  (foldl
   (λ (r fs)
     (match r
       [(underway [values (list)] ς)
        (analyze vm ς)]
       [(failure! message)
        (printf "failure: ~a\n" message)
        fs]
       [(returned code)
        (displayln "success")
        (pretty-print code)
        (pretty-print (hash-ref ς 'OnCompletion))
        (pretty-print (hash-ref ς 'RekeyTo))
        #;
        (pretty-print (foldl (λ (id ς) (hash-remove ς id)) ς '(ApprovalProgram ClearStateProgram bytecode)))
        fs]))
   (set)
   ((vm 'step) ς)))

#;
(require (prefix-in d: "disassemble.rkt"))

(define (run #:approval-program      approval-program
             #:clear-state-program   clear-state-program
             #:global-num-byte-slice global-num-byte-slice
             #:global-num-uint       global-num-uint
             #:local-num-byte-slice  local-num-byte-slice
             #:local-num-uint        local-num-uint
             #:global-state          global-state
             #:mapped-constants      mapped-constants)
  #;
  (for-each
   (λ (d) (displayln (d:directive-line d)))
   (d:disassemble-bytes approval-program))
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
                  ς)))]))
  
  #;
             (run 
              `(¬ 3))
             #;
             (run 
              `3)
             #;
             42
             #;
  (match ((read-varuint prefix-ReadByte) bytecode)
    [(cons lsv bytecode)
     (if (<= lsv 3)
       (analyze lsv bytecode on-completion)
       (error 'standard "does not support LogicSigVersion = ~a > 3" lsv))
     (raise lsv)]
    [#f
     (error 'standard "unable to read initial logic signature version")]))

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
     (run #:approval-program      (base64-decode (string->bytes/utf-8 approval-program))
          #:clear-state-program   (base64-decode (string->bytes/utf-8 clear-state-program))
          #:global-num-byte-slice global-num-byte-slice
          #:global-num-uint       global-num-uint
          #:local-num-byte-slice  local-num-byte-slice
          #:local-num-uint        local-num-uint
          #:global-state          (for/hash ([entry (in-list global-entries)])
                                    (match entry
                                      [(hash-table ('key key) ('value value))
                                       (values (base64-decode (string->bytes/utf-8 key))
                                               (match (hash-ref value 'type)
                                                 [1 (base64-decode (string->bytes/utf-8 (hash-ref value 'bytes)))]
                                                 [2 (hash-ref value 'uint)]))]))
          #:mapped-constants constants)]
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
          (analyze/json-package (call-with-input-file filename port->bytes) (hash))))
      filenames)]))
