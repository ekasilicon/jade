#lang racket/base
(require (only-in racket/match match match-lambda match-let failure-cont ==)
         (only-in racket/list append-map)
         (only-in racket/string string-join)
         racket/set
         "../static/object.rkt"
         "../static/record.rkt"
         "../static/sumtype.rkt"
         "../jade-error.rkt"
         "../report.rkt"
         "../monad.rkt"
         "../assembly.rkt"
         "../assembly/control.rkt"
         "../vm.rkt"
         (prefix-in i: "../instruction.rkt")
         "../instruction/version.rkt"
         "../instruction/control.rkt"
         (rename-in "../abstraction.rkt"
                    [application-id% txn:application-id%]
                    [rekey-to%       txn:rekey-to%]
                    [on-completion%  txn:on-completion%]))
         
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
   vm-pseudo
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
         (λ (x)
           (>>= (get 'mapped-constants)
                (λ (constants)
                  (cond
                    [(hash-ref constants x #f)
                     => (λ (id) (unit `(constant 0 ,x)))]
                    [else
                     (unit x)]))))]
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
                 (panic "byte string ~v exceeds 4096 bytes" r)
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
             (with-handlers ([jade-error? (λ (e) e)])
               (letrec ([loop (λ (seen finals ς ctx)
                                (if (> (set-count seen) 10000)
                                  (jade-error 'UPA "timeout: exceeded 10000 states")
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

(define (report reports)
  (string-append
   "Unconstrained Property Analysis"
   "\n\n"
   (string-join
    (map
     (match-lambda
       [(cons _ report)
        report])
     reports)
    "\n")))

(define (UPA asm ctx mapped-constants)
  (match (control-flow-graph asm)
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
                                  #f))])
         (report (assess ctxs)))
       (jade-error 'unsupported-logic-sig-version "does not support LogicSigVersion = ~a > 3" lsv))]))


(define (assess ctxs)
  (let ([txns (for/set ([ctx (in-set ctxs)])
                (match-let ([(list txn glbl glbl-state) ctx])
                  txn))])
    (cond
      [(zero? (set-count txns))
       (list (cons 2 (format #<<MSG
~a

Under the given environment constraints, the program
CANNOT complete successfully.

If you are sure that the program can complete successfully
under the given environment constraints, then Jade is in
error. Please notify the Jade maintainers via GitHub.

MSG
                             CRITICAL)))]
      [else
       (define-syntax-rule (⇒ A B) (or (not A) B))
       (define (triage txns f)
         (list-ref (sort (for/list ([txn (in-set txns)]) (f txn)) > #:key car) 0))
       (list
        (triage
         txns
         (λ (txn)
           (let ([on-completion (hash-ref txn 'on-completion)]
                 [application-id (hash-ref txn 'application-id)])
             (cond
               [(⇒ (> (set-count on-completion) 1)
                   (equal? application-id '(= 0)))
                (cons 0 (format #<<MSG
OnCompletion ~a

Any successful execution in which multiple OnCompletion
values are allowed must occur during contract creation
(as ApplicationID is properly constrained to be 0).

MSG
                                OK))]
               [(and (= (set-count on-completion) 5)
                     (not (equal? application-id '(= 0))))
                                        ; a stronger form of the condition that follows
                (cons 2 (format #<<MSG
OnCompletion ~a

The OnCompletion property is not constrained *at all*
in standard contract executions!
MSG
                                CRITICAL))]
               [(not (⇒ (> (set-count on-completion) 1)
                        (equal? application-id '(= 0))))
                (cons 1 (format #<<MSG
OnCompletion ~a

The OnCompletion property is only partially constrained
on non-creation contract executions. In particular, its
value can be any in the set

  { ~a }

~a.
MSG
                                ALERT
                                (string-join (map number->string (sort (set->list on-completion) <)) ", ")
                                (match application-id
                                  [#f "during any execution"]
                                  ['(= 0) "when the contract is being created"]
                                  ['(≠ 0) "during a non-creation execution"])))]
               [else
                (raise 'UNREACHABLE)]))))
        (triage
         txns
         (λ (txn)
           (match (hash-ref txn 'rekey-to)
             [#f
              (cons 1 (format #<<MSG
RekeyTo ~a

The RekeyTo property is not constrained by the program.
MSG
                              ALERT))]
             [(i:ZeroAddress)
              (cons 0 (format #<<MSG
RekeyTo ~a

All executions constrain RekeyTo to ZeroAddress.
MSG
                              OK))]))))])))   

(provide UPA)
