#lang racket/base
(require racket/match
         racket/set
         (only-in racket/list append-map)
         "static/sumtype.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "logic-sig-version.rkt"
         "arithmetic-logic-unit.rkt"
         "internal-transaction.rkt"
         "prefix.rkt"
         "vm.rkt"
         (prefix-in i: "instruction.rkt"))

(define-sumtype Result
  (underway values ς)
  (failure! message)
  (returned code))

(define ((unit . xs) ς) (list (underway [values xs] ς)))
(define ((>>= m f) ς)
  (append-map
   (λ (r)
     (sumtype-case Result r
       [(underway [values xs] ς)
        ((apply f xs) ς)]
       [else
        (list r)]))
   (m ς)))

(define ((return code) ς)
  (list (returned code)))
(define ((panic template . args) ς)
  (list (failure! [message (apply format template args)])))

(define standard-Monad (Monad unit >>=))

(define standard-Monad+ (Monad+ [monad standard-Monad]
                                [mplus (λ ms (λ (ς) (append-map (λ (m) (m ς)) ms)))]))

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

(require "unimplemented.rkt")

(define-syntax p
  (syntax-rules ()
    [(_ who n)
     (λ xs (apply unit (build-list n (λ (i) (list* 'who i xs)))))]
    [(_ who)
     (p who 1)]))

(define standard-VM
  (match-let ([(Monad+ [monad (Monad unit >>=)] [mplus each]) standard-Monad+]
              [>> (>> standard-Monad)]
              [fail (mzero standard-Monad+)])
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
     [in-mode (λ (target-mode info)
                (>>= (get mode #f)
                     (match-lambda
                       [#f
                        (put mode target-mode)]
                       [mode
                        (if (eq? mode target-mode)
                          (unit)
                          (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))]
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
     [get-pc (get pc)]
     [set-pc (λ (pc) (put pc pc))]
     [get-bytecode (get bytecode)]
     [get-intcblock (get intcblock)]
     [put-intcblock (λ (xs) (put intcblock xs))]
     [get-bytecblock (get bytecblock)]
     [put-bytecblock (λ (bss) (put bytecblock bss))]
     [arg (p arg 1)]
     [push-call ()]
     [pop-call ()]
     #|
     [&& (λ (s₀ s₁) (unit `(∧ ,s₀ ,s₁)))]
     [\|\| (λ (s₀ s₁) (unit `(∨ ,s₀ ,s₁)))]
     [!
      (match-lambda
        [`(¬ ,s) (unit s)]
        [s (unit `(¬ ,s))])]
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
     [get-pc (get pc)]
     [set-pc (λ (pc) (put pc pc))]
     [get-bytecode (get bytecode)]
     [get-intcblock (get intcblock)]
     [put-intcblock (λ (xs) (put intcblock xs))]
     [get-bytecblock (get bytecblock)]
     [put-bytecblock (λ (bss) (put bytecblock bss))]
     [is-zero (λ (x) (symbolic-if x (unit #f) (unit #t)))]
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
        (define-syntax-rule (enumtype-case type e [(recname ...) body ...] ... [else else-body ...])
          (sumtype-case type e
            [((recname) ...) body ...] ... [else else-body ...]))
        (enumtype-case i:TransactionField f
          [(i:Sender i:OnCompletion i:TypeEnum i:GroupIndex)
           (unit f)]
          [(i:ApplicationID i:NumAppArgs i:NumAccounts i:RekeyTo)
           (>> ((logic-sig-version>= standard-VM) 2 (i:transaction-field-name f))
               (unit f))]
          [else
           (error 'transaction "handle case for ~a" f)]))]
     [group-transaction
      (λ (gi f)
        (unit `(txn-property ,gi ,f)))]
     group-transaction-array
     [transaction-array
      (λ (f ai) (group-transaction-array (i:GroupIndex) f ai))]
     [global
      (λ (f)
        (define-syntax-rule (enumtype-case type e [(recname ...) body ...] ... [else else-body ...])
          (sumtype-case type e
            [((recname) ...) body ...] ... [else else-body ...]))
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
          [else
           (error 'global "handle case for ~a" f)]))]
     [balance
      (λ (acct)
        (>> (assume (<rw `(<= (+ ,(i:NumApplications)
                                 (+ ,(i:NumAccounts)
                                    ,(i:NumAssets)))
                              8)))
            (symbolic-if (<rw `(<= ,acct ,(i:NumAccounts)))
                         )))]
     [app-opted-in
      (λ (acct app)
        (unit `(app-opted-in ,acct ,app)))]
     [app-global-get
      (λ (key)
           (>>= (get app-global (list))
                (letrec ([lookup (match-lambda
                                   [(list)
                                    (unit `(app-global ,key))]
                                   [(cons `(put ,put-key ,val)
                                          ag)
                                    (symbolic-if `(== ,key ,put-key)
                                                 (unit val)
                                                 (lookup ag))])])
                  lookup)))]
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
     [app-global-put
      (λ (key val) (update app-global (λ (ag) (cons `(put ,key ,val) ag)) (list)))]
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
     [asset-holding-get
      (λ (acct asst f)
        (unit `(asset-holding ,acct ,asst ,f)))]
     [load
      (λ (i)
        (>>= (get scratch-space (hasheqv))
             (λ (ss)
               (cond
                 [(hash-ref ss i #f) => unit]
                 [else
                  (>> (log "Scratch space slot ~a uninitialized. The Go VM implementation produces 0." i)
                      (unit 0))]))))]
     [store (λ (i x) (update scratch-space (λ (ss) (hash-set ss i x)) (hasheqv)))]
     |#
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

(define (analyze ς)
  (foldl
   (λ (r fs)
     (match r
       [(underway [values (list)] ς)
        (analyze ς)]
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
   ((step standard-VM) ς)))

(require (prefix-in d: "disassemble.rkt"))

(define (run #:approval-program      approval-program
             #:clear-state-program   clear-state-program
             #:global-num-byte-slice global-num-byte-slice
             #:global-num-uint       global-num-uint
             #:local-num-byte-slice  local-num-byte-slice
             #:local-num-uint        local-num-uint
             #:global-state          global-state
             #:mapped-constants      mapped-constants)
  (pretty-print (disassemble-bytes approval-program))
  (let ([ς (hasheq 'ApprovalProgram    approval-program
                   'ClearStateProgram  clear-state-program
                   'GlobalNumByteSlice global-num-byte-slice
                   'GlobalNumUint      global-num-uint
                   'LocalNumByteSlice  local-num-byte-slice
                   'LocalNumUint       local-num-uint
                   'GlobalState        global-state
                   'MappedConstants    mapped-constants)])
    (match ((read-varuint prefix-ReadByte) approval-program)
      [(cons lsv bytecode)
       (analyze (let* ([ς (hash-set ς 'LogicSigVersion lsv)]
                       [ς (hash-set ς 'OnCompletion    `(≠ 3))]
                       [ς (hash-set ς 'RekeyTo         #f)]
                       [ς (hash-set ς 'bytecode        bytecode)]
                       [ς (hash-set ς 'pc              0)]
                       [ς (hash-set ς 'stack           (list))]
                       [ς (hash-set ς 'scratch-space   (hasheqv))]
                       [ς (hash-set ς 'intcblock       (list))]
                       [ς (hash-set ς 'bytecblock      (list))])
                  ς))]))
  
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

(module+ main
  (require json
           net/base64)

  (require racket/port
           "disassemble.rkt")
  
  (match (current-command-line-arguments)
    [(vector filenames ...)
     (for-each
      (λ (filename)
        (displayln filename)
        (with-handlers (#;[exn:fail? (λ (e) (displayln (exn-message e)))]
                        )
          (analyze/json-package (call-with-input-file filename port->bytes) (hasheq))))
      filenames)]))
