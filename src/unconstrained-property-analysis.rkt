#lang racket/base

#|

The unconstrained property (not parameter) analysis identifies
properties which the contract allows to vary and, in some cases,
the conditions under which it does so.

It is achieved by a series of increasingly sophisticated, precise, 
and costly analyses.

Consider this cheap analysis for the fields OnCompletion and 
RekeyTo in particular.
The analyzer interprets all arithmetic/logic instructions
symbolically, so that the stack contains symbolic expressions.
When a branch is encountered, the symbolic expression serving 
as the guard is scanned for an occurrence of OnCompletion or 
RekeyTo.
If one is found, the symbolic expression is interpreted---
matched against one of several simple known forms---to refine
the abstract value of the field in the machine state.

For example, when an ApprovalProgram runs, the abstract value 
of the OnCompletion property is { 0, 1, 2, 4, 5 }.
Suppose the program contains the sequence

txn OnCompletion
push 1
==
txn OnCompletion
push 2
==
||
txn OnCompletion
push 4
==
||
txn OnCompletion
push 5
||
bz good
err
good:
...

Upon encountering the `bz` instruction, the symbolic expression is

(∨ (∨ (∨ (= OnCompletion 1)
         (= OnCompletion 2))
      (= OnCompletion 4))
   (= OnCompletion 5))

Regardless of whether OnCompletion or RekeyTo appears in the expression,
the analyzer proceeds to decide whether it can be zero and nonzero, first
by processing negations, conjunctions, and disjunctions.

To decide whether it can be nonzero is to `assume` it.
When `assume` encounters a disjunction, it splits the world, assuming in
turn each side of the disjunction.
Recurring, the analyzer will split into four parts.
To `assume` (= OnCompletion x) is to constrain the OnCompletion abstract
value to be its current value intersected with { x }.
If the result is the empty set, then the assumption is false.
If the result is not the empty set, then the assumption can be satisfied
with the non-empty set as the newly-assumed value.
Each of these four cases leads to the contract executing `err`, 
an unsuccessful execution of the program.

To decide whether it can be zero is to `refute` it, or to `assume` its
negation.
`assume` implements that `(¬ (∨ A B))` ≡ `(∧ (¬ A) (¬ B))` which,
recurring, yields

(∧ (∧ (∧ (¬ (= OnCompletion 1))
         (¬ (= OnCompletion 2)))
      (¬ (= OnCompletion 4)))
   (¬ (= OnCompletion 5)))

A conjunction is handled by processing each side in turn, on the same path.
The interpretation of `(¬ (= OnCompletion 1))` is to subtract the set { x }
from the current value of OnCompletion, i.e., intersect it with the complement
of { x }.
If the result is the empty set, then it cannot be refuted, and the value cannot
be zero.
In this example, the initial value of { 0, 1, 2, 4, 5 } becomes { 0, 2, 4, 5 },
{ 0, 4, 5 }, { 0, 5 }, and then { 0 }, before continuing at label `good`.

The OnCompletion interpretation is dispatched only once the negations, 
conjunctions, and disjunctions have been handled.
If the term does not contain the `OnCompletion` property (or another of interest),
then it can be ignored.
The result is a less-precise analysis, but not a less-safe analysis as ignoring
those terms allows the analysis to explore only more of the execution space.

The stack is not the only place that the `OnCompletion` property could conceivably
be stored.
It could also be stored in storage or even global or local storage.
Thus, these features should be soundly approximated.

The suite of programs from scratch will include strange ways of handling these
properties, including each of these.

If an expression contains `OnCompletion` but it isn't recognized by the interpreter,
the analysis fails (and fortunately can report on exactly what it choked).
Alternatively, this term could be stored in a path condition and reported at the
end.

By not storing any terms which do not contain properties of interest in the path 
condition, the path condition is very sparsely populated and therefore doesn't
increase the state space much.
Only the path condition and the values of the properties of interest increase
the state space, so this should be a very brisk analysis.

|#

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
                                          [constraints (hasheq 'OnCompletion (hash-ref ς 'OnCompletion)
                                                               'RekeyTo      (hash-ref ς 'RekeyTo))])))
(define ((panic template . args) ς) (list (failure! [message (apply format template args)])))

(define ((>>= m f) ς)
  (append-map
   (sumtype-case-lambda Result
     [(underway [values xs] ς)
      ((apply f xs) ς)]
     #:otherwise r
     (list r))
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
      (each (>> (refute c) t)
            (>> (assume c) f)))
    (define (contains? ? c)
      (if (? c)
        #t
        (match c
          [(cons tag cs)
           (ormap (λ (c) (contains? ? c)) cs)]
          [_
           #f])))
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
        [`(== ,(? exact-nonnegative-integer? x)
              ,(? exact-nonnegative-integer? y))
         (if (= x y) (unit) fail)]
        [`(< ,(? exact-nonnegative-integer? x)
             ,(? exact-nonnegative-integer? y))
         (if (< x y) (unit) fail)]
        [c
         (cond
           [(exact-nonnegative-integer? c)
            (if (zero? c) fail (unit))]
           [(contains? (sumtype-predicate i:OnCompletion) c)
            (match c
              [(or `(== ,(i:OnCompletion) ,(? exact-nonnegative-integer? n))
                   `(== ,(? exact-nonnegative-integer? n) ,(i:OnCompletion)))
               (>>= (get OnCompletion)
                    (λ (oc₀)
                      (let ([oc (set-intersect oc₀ (seteqv n))])
                        (if (set-empty? oc)
                          fail
                          (put OnCompletion oc)))))]
              [c
               (put OnCompletion-unsound #t)])]
           [(contains? (sumtype-predicate i:RekeyTo) c)
            (match c
              [`(== ,(i:RekeyTo) ,(i:ZeroAddress))
               (>>= (get RekeyTo)
                    (match-lambda
                      [#f
                       (put RekeyTo `(= ,(i:ZeroAddress)))]
                      [`(= ,(i:ZeroAddress))
                       (unit)
                       #;
                       (cond
                         [(must-be-=? rt₀ (i:ZeroAddress))
                          (unit)]
                         [(may-be-=? rt₀ (i:ZeroAddress))
                          (error 'RekeyTo "Does ZeroAddress refine ~a?" rt₀)]
                         [else
                          fail])]
                      [`(≠ ,(i:ZeroAddress))
                       fail
                       #;
                       (if (for/and ([rt (in-set rts)])
                             (must-be-≠? rt (i:ZeroAddress)))
                         (put RekeyTo (i:ZeroAddress))
                         (error 'RekeyTo "Could ZeroAddress be in ~a?" rts))]))]
              [c
               (put RekeyTo-unsound #t)])]
           [else
            (eprintf "ignoring constraint\n~a\n" c)
            (unit)])]))
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
        [`(== ,(? exact-nonnegative-integer? x)
              ,(? exact-nonnegative-integer? y))
         (if (= x y) fail (unit))]
        [`(< ,(? exact-nonnegative-integer? x)
             ,(? exact-nonnegative-integer? y))
         (if (< x y) fail (unit))]
        [c
         (cond
           [(exact-nonnegative-integer? c)
            (if (zero? c) (unit) fail)]
           [(contains? (sumtype-predicate i:OnCompletion) c)
            (match c
              [(or `(== ,(i:OnCompletion) ,(? exact-nonnegative-integer? n))
                   `(== ,(? exact-nonnegative-integer? n) ,(i:OnCompletion)))
               (>>= (get OnCompletion)
                    (λ (oc₀)
                      ; (set-intersect oc₀ (set-complement (seteq 0 1 2 4 5) (set n)))
                      (let ([oc (set-subtract oc₀ (seteqv n))])
                        (if (set-empty? oc)
                          fail
                          (put OnCompletion oc)))))]
              [c
               (put OnCompetion-unsound #t)])]
           [(contains? (sumtype-predicate i:RekeyTo) c)
            (match c
              [`(== ,(i:RekeyTo) ,(i:ZeroAddress))
               (>>= (get RekeyTo)
                    (match-lambda
                      [#f
                       (put RekeyTo `(≠ ,(i:ZeroAddress)))]
                      [`(= ,(i:ZeroAddress))
                       fail
                       #;
                       (cond
                         [(must-be-≠? rt₀ (i:ZeroAddress))
                          (unit)]
                         [(may-be-≠? rt₀ (i:ZeroAddress))
                          (error 'RekeyTo "Does ZeroAddress refine ~a?" rt₀)]
                         [else
                          fail])]
                      [`(≠ ,(i:ZeroAddress))
                       (unit)
                       #;
                       (if (for/and ([rt (in-set rts)])
                             (must-be-≠? rt (i:ZeroAddress)))
                         (put RekeyTo (i:ZeroAddress))
                         (error 'RekeyTo "Could ZeroAddress be in ~a?" rts))]))]
              [c
               (put RekeyTo-unsound #t)
               (unit)])]
           [else
            (eprintf "ignoring constraint\n~a\n" c)
            (unit)])]))
    (VM monad+
        read-byte
        logic-sig-version
        panic
        return
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
        [is-zero (λ (c) (sif c (unit #t) (unit #f)))]
        [&& (s 2 &&)]
        [\|\| (s 2 \|\|)]
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
        [global
         (sumtype-case-lambda i:GlobalField
           [(i:GroupSize)
            (let loop ([n 16])
              (if (zero? n)
                fail
                (each (unit n)
                      (loop (sub1 n)))))]
           #:otherwise f (unit f))]
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
                         (unit))))))])))


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
                                                      [(returned)
                                                       (values todo (set-add final r))]
                                                      [(failure! message)
                                                       #;(displayln message)
                                                       (values todo final)]))])
                         (loop todo (set-add seen ς) final)))])
                  (fk "exceeded ~a states" capacity)))))
          fk))

(module+ main
  (require racket/pretty
           "../test/algoexplorer/extract.rkt")
  (for/sum ([path (in-list (vector->list (current-command-line-arguments)))])
    (let ([bytecode (file-extract path 'approval-program)])
      (displayln path)
      (analyze bytecode (λ (final) 1) (λ (template . args) (eprintf "~a\n" (apply format template args)) 0)))))
