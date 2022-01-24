#lang racket/base
(require (only-in racket/match match match-lambda match-let failure-cont ==)
         (only-in racket/list append-map)
         racket/set
         "static/object.rkt"
         "static/record.rkt"
         "static/sumtype.rkt"
         "error.rkt"
         "monad.rkt"
         "assembly.rkt"
         "disassemble.rkt"
         ;"parse.rkt"
         "assembly/control.rkt"
         "vm.rkt"
         "instruction/version.rkt"
         "instruction/control.rkt"
         (prefix-in txn: "abstraction.rkt"))

(define-sumtype Result
  (underway values ς ctx)
  (failure! message)
  (returned code ctx))

(define-syntax p
  (syntax-rules ()
    [(_ unit who n)
     (λ xs (apply unit (build-list n (λ (i) (list* 'who i xs)))))]
    [(_ unit who)
     (p who 1)]))

(define concrete-stack%
  (inc (unit >>= >>
        panic
        get put upd)
       [push
        (λ (x) (upd 'stack (λ (stk) (cons x stk)) (list)))]
       [pop
        (>>= (get 'stack)
             (match-lambda
               [(cons x stk)
                (>> (put 'stack stk)
                    (unit x))]
               [(list)
                (panic "tried to pop an empty stack")]))]))

(define concrete-cblock%
  (inc (get put)
       [get-intcblock
        (get 'intcblock)]
       [put-intcblock
        (λ (xs) (put 'intcblock xs))]
       [get-bytecblock
        (get 'bytecblock)]
       [put-bytecblock
        (λ (bss) (put 'bytecblock bss))]))

(define standard-in-mode%
  (inc (unit >>= panic
        get put)
       [in-mode
        (λ (target-mode info)
          (>>= (get 'mode #f)
               (match-lambda
                 [#f
                  (put 'mode target-mode)]
                 [mode
                  (if (eq? mode target-mode)
                    (unit)
                    (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))]))

(define (make-abstraction% . abs%s)
  (let ([abss (map fix abs%s)])
    (inc (unit >> mplus mzero)
         [initialize-context
          (foldl
           (λ (abs m)
             (>> m ((abs 'initialize-context) self)))
           (unit)
           abss)]
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
             (foldr (λ (abs m) (>> (((abs 'assume) c) self) m)) (unit) abss)])]
         [refute
          (match-lambda
            [`(! 0 ,c) (assume c)]
            [`(&& 0 ,c₀ ,c₁)
             (mplus (refute c₀)
                    (refute c₁))]
            [`(\|\| 0 ,c₀ ,c₁)
             (>> (refute c₀)
                 (refute c₁))]
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
             (foldr (λ (abs m) (>> (((abs 'refute) c) self) m)) (unit) abss)])])))

(define logical-connective%
  (inc (unit)
       [!
        (λ (x)
          (if (exact-nonnegative-integer? x)
            (unit (if (zero? x) 1 0))
            (unit `(! 0 ,x))))]
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
            (unit `(\|\| 0 ,x ,y))))]))

(define (make-vm lsv)
  #;
  (define-values (offset-map has-inorder-successor?)
         (let ([o (fix (mix (inc ()
                                 [offset-map
                                  (λ (f instr)
                                    (sumtype-case Pseudoinstruction instr
                                      [(varuint-immediate value)
                                       (varuint-immediate value)]
                                      [(bytes-immediate value)
                                       (bytes-immediate value)]
                                      [(instruction [instruction instr])
                                       (instruction [instruction ((super 'offset-map) f instr)])]))]
                                 [has-inorder-successor?
                                  (sumtype-case-lambda Pseudoinstruction
                                    [(varuint-immediate)
                                     #t]
                                    [(bytes-immediate)
                                     #t]
                                    [(instruction [instruction instr])
                                     (instruction [instruction ((super 'has-inorder-successor?) instr)])])])
                            (instruction-control/version lsv)))])
           (values (o 'offset-map)
                   (o 'has-inorder-successor?))))
  (mix
   #;
   (inc (>> trace)
        [step
         (>> (trace (λ (ς _) (pretty-print
                              (hash-update ς 'pc (letrec ([go (match-lambda
                                                                [(list* instr₀ instr₁ _)
                                                                 (list* (offset-map go instr₀)
                                                                        (offset-map go instr₁)
                                                                        (list))]
                                                                [(list* instr₀ _)
                                                                 (list* (offset-map go instr₀)
                                                                        (list))]
                                                                [(list)
                                                                 (list)])])
                                                   go)))))
             (super 'step))])
   
   (vm/version lsv)
   concrete-stack%
   concrete-cblock%
   (make-abstraction%
    txn:application-id%
    txn:rekey-to%
    txn:on-completion%)
   standard-in-mode%
   logical-connective%
   (inc (unit >>= >> mplus mzero
         assume refute
         get put upd )
        [panic
         (λ (template . args)
           (λ (ς ctx) (list (failure! [message (apply format template args)]))))]
        [return
         (λ (code)
           (if0 code
                (λ (ς ctx) (list (returned [code 0] ctx)))
                (λ (ς ctx) (list (returned [code 1] ctx)))))]
        [check-final
         (>>= (get 'pc)
              (match-lambda
                [(list)
                 (>>= (get 'stack (list))
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
        [itob
         (λ (x)
           (if (exact-nonnegative-integer? x)
             (unit (string->bytes/utf-8 (number->string x)))
             (unit `(itob 0 ,x))))]
        [concat
         (λ (x y)
           (if (and (bytes? x)
                    (bytes? y))
             (let ([r (bytes-append x y)])
               (if (> (bytes-length r) 4096)
                 (panic "byte string ~v exceeds 4096 bytes")
                 (unit r)))
             (unit `(concat 0 ,x ,y))))]
        [substring (p unit substring 1)]
        [len (p unit len 1)]
        [getbyte (p unit getbyte 1)]
        [setbyte (p unit setbyte 1)]
        [balance (p unit balance 1)]
        [min-balance (p unit min-balance 1)]
        [sha256 (p unit sha256 1)]
        [keccak256 (p unit keccak256 1)]
        [sha512-256 (p unit sha512-256 1)]
        [u== (p unit == 1)]
        [u<  (p unit < 1)]
        [u+ (p unit + 1)]
        [u- (p unit - 1)]
        [u* (p unit * 1)]
        [u/ (p unit / 1)]
        [u& (p unit & 1)]
        [u% (p unit % 1)]
        [u& (p unit & 1)]
        [u\| (p unit \| 1)]
        [u^ (p unit ^ 1)]
        [u~ (p unit ~ 1)]
        [addw (p unit addw 2)]
        [mulw (p unit mulw 2)]
        [app-opted-in (p unit app-opted-in 1)]
        [app-local-get
         (λ (acct key)
           (>>= (get 'app-local (list))
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
           (>>= (get 'app-local (list))
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
           (upd 'app-local (λ (al) (cons `(put ,acct ,key ,val) al)) (list)))]
        [app-local-del
         (p unit app-local-del 1)]
        [app-global-get
         (λ (key)
           (>>= (get 'app-global (list))
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
           (upd 'app-global (λ (ag) (cons `(put ,key ,val) ag)) (list)))]
        [app-global-del
         (p unit app-global-del 1)]
        [app-global-get-ex
         (λ (app key)
           (>>= (get 'app-global (list))
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
           (>>= (get 'scratch-space (hasheqv))
                (λ (ss)
                  (cond
                    [(hash-ref ss i #f) => unit]
                    [else
                     (>> #;(log "Scratch space slot ~a uninitialized. The Go VM implementation produces 0." i)
                         (unit 0))]))))]
        [store (λ (i x) (upd 'scratch-space (λ (ss) (hash-set ss i x)) (hasheqv)))])
   (inc (put)
        [jump
         (λ (cfg) (put 'pc cfg))])
   (instruction-version/version lsv)
   (inc ()
        [instruction-name
         (λ (instr) "<instruction name>")])
   (inc (panic
         unit >>= >>
         get put)
        [read-instruction
         (>>= (get 'pc)
              (match-lambda
                [(list)
                 (panic "attempt to read instruction at end of stream")]
                [(cons instr cfg)
                 (>> (put 'pc cfg)
                     (unit instr))]))])
   (inc (get)
        [logic-sig-version
         (get 'logic-sig-version)])
   (inc (upd)
        [log
         (λ (template . args)
           (let ([msg (apply format template args)])
             (upd 'log (λ (msgs) (set-add msgs msg)) (set))))])
   monad+-extras
   (inc ()
        [mplus
         (λ ms (λ (ς ctx) (apply append (map (λ (m) (m ς ctx)) ms))))])
   monad-extras
   (inc ()
        [unit
          (λ values (λ (ς ctx) (list (underway values ς ctx))))]
        [>>=
         (λ (m f)
           (λ (ς ctx)
             (append-map
              (sumtype-case-lambda Result
                [(underway [values xs] ς ctx)
                 ((apply f xs) ς ctx)]
                #:otherwise list)
              (m ς ctx))))]
        [get
         (let ([absent (string->uninterned-symbol "absent")])
           (λ (key [default absent])
             (λ (ς ctx)
               (list (underway [values (list (if (eq? default absent)
                                               (hash-ref ς key)
                                               (hash-ref ς key default)))]
                               ς ctx)))))]
        [put
         (λ (key val)
           (λ (ς ctx)
             (list (underway [values (list)] [ς (hash-set ς key val)] ctx))))]
        [upd
         (let ([absent (string->uninterned-symbol "absent")])
           (λ (key f [default absent])
             (λ (ς ctx)
               (list (underway [values (list)]
                               [ς (if (eq? default absent)
                                    (hash-update ς key f)
                                    (hash-update ς key f default))]
                               ctx)))))]
        [transaction-property-get
         (λ (key)
           (λ (ς ctx)
             (match-let ([(list txn glbl glbl-state) ctx])
               (list (underway [values (list (hash-ref txn key))] ς ctx)))))]
        [transaction-property-put
         (λ (key val)
           (λ (ς ctx)
             (match-let ([(list txn glbl glbl-state) ctx])
               (list (underway [values (list)]
                               ς
                               [ctx (list (hash-set txn key val)
                                          glbl
                                          glbl-state)])))))]
        [trace
         (λ (f)
           (λ (ς ctx)
             (f ς ctx)
             (list (underway [values (list)] ς ctx))))])))

(require racket/pretty)

(define (analyze vm ς ctx)
  (let ([→ (vm 'step)])
    (match ((vm 'initialize-context) ς ctx)
      [(list (underway [values (list)] ς ctx))
       (define ch (make-channel))
       (define th
         (thread
          (λ ()
            (channel-put
             ch
             (let/ec return
               (letrec ([loop (λ (seen finals ς ctx)
                                (if (> (set-count seen) 10000)
                                  (return (error 'timeout "exceeded 10000 states"))
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
                                                   [0 finals]))])))))])
                 (let-values ([(seen finals) (loop (set) (set) ς ctx)])
                   finals)))))))
       (cond
         [(sync/timeout 10 ch)
          => values]
         [else
          (kill-thread th)
          (error 'timeout "exceeded 10 seconds")])])))

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
     (match (control-flow-graph (disassemble approval-program))
       [(cons lsv cfg)
        (if (<= lsv 3)
          (match (analyze (fix (make-vm lsv))
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
                                     global-state))
            [(error-result tag message)
             (error-result tag message)]
            [ctxs
             (for/set ([ctx (in-set ctxs)])
               (match-let ([(list txn glbl glbl-state) ctx])
                 txn))]) 
          (error 'unsupported-logic-sig-version "does not support LogicSigVersion = ~a > 3" lsv))]
       [#f
        (error 'bad-binary-prefix "unable to read initial logic signature version")])]))

(define (analyze/raw-binary program-type bs constants)
  42)

(require json
         net/base64)

(define (analyze/json-package bs constants)
  (match (with-handlers ([exn:fail:read? (λ (e) (error 'invalid-json (exn-message e)))])
           (read-json (open-input-bytes bs)))
    [(error-result tag message)
     (error-result tag message)]
    [(hash-table ('id id)
                 ('params (and params
                               (hash-table ('approval-program    approval-program)
                                           ('clear-state-program clear-state-program)
                                           ('creator             creator)
                                           ('global-state-schema (hash-table ('num-byte-slice global-num-byte-slice)
                                                                             ('num-uint       global-num-uint)))
                                           ('local-state-schema  (hash-table ('num-byte-slice local-num-byte-slice)
                                                                             ('num-uint       local-num-uint)))))))
     (if (or (eq? approval-program 'null)
             (eq? clear-state-program 'null))
       (error 'program-missing "program was null in package")
       (let ([global-state (match params
                             [(hash-table ('global-state global-entries))
                              (for/hash ([entry (in-list global-entries)])
                                (match entry
                                  [(hash-table ('key key) ('value value))
                                   (values (base64-decode (string->bytes/utf-8 key))
                                           (match (hash-ref value 'type)
                                             [1 (base64-decode (string->bytes/utf-8 (hash-ref value 'bytes)))]
                                             [2 (hash-ref value 'uint)]))]))]
                             [_
                              #f])])
         (run (execution-context [approval-program     (base64-decode (string->bytes/utf-8 approval-program))]
                                 [clear-state-program  (base64-decode (string->bytes/utf-8 clear-state-program))]
                                 global-num-byte-slice
                                 global-num-uint
                                 local-num-byte-slice
                                 local-num-uint
                                 global-state)
              constants)))]
    [json
     (error 'invalid-package-format "Input JSON did not match expected format. (Was it produced by the Algorand API v2?)")]))

(provide analyze/raw-binary
         analyze/json-package)

#;
(define (analyze/assembly asm)
  (match (control-flow-graph (parse asm))
    [(cons lsv cfg)
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
       (error 'unsupported-logic-sig-version "does not support LogicSigVersion = ~a > 3" lsv))]
    [#f
     (error 'assembly-parse-failure "unable to parse assembly")]))

#;
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
        (match (analyze/json-package (call-with-input-file filename port->bytes) (hash))
          [(error-result tag message)
           (displayln tag)
           (displayln message)]
          [results
           (pretty-print results)]))
      filenames)]))
