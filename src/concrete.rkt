#lang racket/base
(require (only-in racket/match match match-lambda failure-cont)
         "monad.rkt"
         "disassemble.rkt"
         "vm.rkt"
         "abstraction.rkt")

(define-sumtype Result
  (underway values ς ctx)
  (failure! message)
  (returned code ctx))

(define (make-vm lsv)
  (mix
   (vm/version lsv)
   concrete-stack%
   concrete-cblock%
   standard-in-mode%
   logical-connective%
   (inc (unit >>= >> mplus mzero
         assume refute
         get put upd)
        [panic
         (λ (template . args)
           (λ (ς ctx) (failure! [message (apply format template args)])))]
        [return
         (λ (code)
           (if0 code
                (λ (ς ctx) (returned [code 0] ctx))
                (λ (ς ctx) (returned [code 1] ctx))))]
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
           (if (exact-nonnegative-integer? x)
             (if (zero? x) m₀ m₁)
             (panic "expected uint but got ~a" x)))]
        [is-zero
         (λ (x) (if0 x (unit #t) (unit #f)))]
        [transaction
         (match-lambda)]
        [group-transaction
         (λ (index field)
           (match field))]
        [transaction-array
         (λ (field index)
           (match field))]
        [group-transaction-array
         (λ (group-index field array-index)
           (match field))]
        [global
         (match-lambda)]
        [btoi
         (λ (bs)
           (if (bytes? bs)
             (if (<= (bytes-length bs) 8)
               (unit (for/fold ([x 0]) ([b (in-bytes bs)]) (+ (* x 256) b)))
               (panic "length of bytes is greater than 8: ~v" bs))
             (panic "btoi: not bytes: ~v" bs)))]
        [itob
         (λ (x)
           (if (exact-nonnegative-integer? x)
             (unit (apply bytes
                          (reverse
                           (let loop ([x x]
                                      [i 8])
                             (if (zero? i)
                               (list)
                               (cons (remainder x 256)
                                     (loop (quotient x 256)
                                           (sub1 i))))))))
             (panic "itob: not integer: ~a" x)))]
        [concat
         (λ (x y)
           (if (and (bytes? x)
                    (bytes? y))
             (let ([r (bytes-append x y)])
               (if (> (bytes-length r) 4096)
                 (panic "byte string ~v exceeds 4096 bytes" r)
                 (unit r)))
             (panic "concat: expected bytes but got ~v and ~v" x y)))]
        [substring
         (λ (bs s e)
           (if (bytes? bs)
             (if (and (<= s e)
                      (<= e (bytes-length bs)))
               (unit (substring bs s e))
               (panic "substring: bad range for ~v, start: ~v end: ~v" bs s e))
             (panic "substring: expected bytes but got ~v" bs)))]
        [len
         (λ (bs)
           (if (bytes? bs)
             (unit (bytes-length bs))
             (panic "len: expected bytes but got ~v" bs)))]
        [getbyte
         (λ (idx bs)
           (if (and (bytes? bs)
                    (exact-nonnegative-integer? idx))
             (if (< idx (bytes-length bs))
               (unit (bytes-ref bs idx))
               (panic "getbyte: index ~v out of range for ~v" idx bs))
             (panic "getbyte: expected bytes and index but got ~v and ~v" bs idx)))]
        [setbyte
         (λ (b idx bs)
           (cond
             [(> (uint-value b) 255)
              (panic "setbyte value > 255")]
             [(>= (uint-value idx) (bytes-length (bytes-value bs)))
              (panic "setbyte index beyond array length")]
             [else
              (unit (bytes-append (subbytes (bytes-value bs) 0 (uint-value idx))
                                  (bytes (uint-value b))
                                  (subbytes (bytes-value bs) (add1 (uint-value idx)))))])) (p unit setbyte 1)]
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


; logic/eval.go
(define (eval asm eval-ctx)
  (error->>= (control-flow-graph asm)
             (match-lambda
               [(cons lsv cfg)
                (define max-version 3)
                (cond
                  [(> lsv max-version)
                   (error "program version ~a greater than max supported version ~a" lsv max-version)]
                  [(> lsv (eval-ctx 'max-version))
                   (error "program version ~a greater than protocol supported version ~a" lsv (eval-ctx 'max-version))]
                  [(< lsv (eval-ctx 'min-version))
                   (error "program version must be >= %d for this transaction group, but have version %d" lsv (eval-ctx 'min-version))]
                  [else
                   (error->>= (analyze (fix (make-vm lsv))
                                       (hasheq 'logic-sig-version lsv
                                               'pc                cfg
                                               'stack             (list)
                                               'scratch-space     (hasheqv)
                                               'intcblock         (list)
                                               'bytecblock        (list))
                                       (list (hasheq)
                                             #f
                                             #f))
                             )
           (actually-run-the-program)])
                (if (<= lsv 3)
                  
                  (error 'unsupported-logic-sig-version "does not support LogicSigVersion = ~a > 3" lsv))])))
#;
(match (((fix (mix read-byte-extras
                        (inc ()
                             [read-byte
                              ...])))
              'read-varuint)
             bytecode)
  [(cons lsv bytecode)
   ]
       [#f
        (error "invalid version")]))

; see dryrunCmd in clerk.go
(define (execute txn-grp txn i)
  )

(define (dry-run txn-grp)
  (for/fold ()
            ([txn (in-list txn-grp)]
             [i (in-naturals)])
    ; need to return scratch space?
    
    (execute txn-grp txn i)))

(provide dry-run)
