#lang racket/base
(require (only-in racket/match match match-lambda failure-cont)
         racket/set
         racket/pretty
         "static/object.rkt"
         "prefix.rkt"
         "monad.rkt"
         "vm.rkt"
         "instruction/version.rkt"
         "instruction/name.rkt")

(define (fail ς) 0)
(define ((return x) ς) x)

(define ((get k) ς)
  (cons (list (hash-ref ς k)) ς))
(define (((put k) v) ς)
  (cons (list) (hash-set ς k v)))
(define ((upd k f) ς)
  (cons (list) (hash-update ς k f)))

(define debug
  (match-lambda
    [(cons lsv cfg)
     (let ([→ ((fix (mix (vm/version lsv)
                         (instruction-version/version lsv)
                         (instruction-name/version lsv)
                         monad-extras
                         (inc (>>)
                              [oracle
                               (λ (name)
                                 (λ xs
                                   (λ (ς)
                                     (displayln name)
                                     (for-each println xs)
                                     (let ([x (read)])
                                       ((>> (upd 'props (λ (ps) (set-add ps `(= ,(cons name xs) ,x))))
                                            (unit x))
                                        ς)))))]
                              [unit
                               (λ xs (λ (ς) (cons xs ς)))]
                              [>>=
                               (λ (m f)
                                 (λ (ς)
                                   (match (m ς)
                                     [(cons xs ς)
                                      ((apply f xs) ς)]
                                     [(? exact-nonnegative-integer? x) x])))]
                              [constant
                               unit
                               #;
                               (λ (v)
                                 (λ (ς)
                                   (displayln "CONSTANT")
                                   (println v)
                                   (cons (list (read)) ς)))]
                              [push
                               (λ (x) (upd 'stack (λ (stk) (cons x stk))))]
                              [pop
                               (>>= (get 'stack)
                                    (match-lambda
                                      [(list)
                                       fail]
                                      [(cons x stk)
                                       (>> ((put 'stack) stk)
                                           (unit x))]))]
                              [in-mode
                               (λ (mode instr) (unit))]
                              [is-zero
                               (λ (x)
                                 (if (exact-nonnegative-integer? x)
                                   (unit (zero? x))
                                   ((oracle 'is-zero) x)))]
                              [u==
                               (λ (x y)
                                 (cond
                                   [(and (exact-nonnegative-integer? x)
                                         (exact-nonnegative-integer? y))
                                    (unit (if (= x y) 1 0))]
                                   [(and (bytes? x)
                                         (bytes? y))
                                    (unit (if (bytes=? x y) 1 0))]
                                   [else
                                    ((oracle '==) x y)]))]
                              [u+
                               (λ (x y)
                                 (if (and (exact-nonnegative-integer? x)
                                          (exact-nonnegative-integer? y))
                                   (let ([z (+ x y)])
                                     (if (< z (expt 2 64))
                                       (unit z)
                                       fail))
                                   ((oracle '+) x y)))]
                              [u<
                               (λ (x y)
                                 (if (and (exact-nonnegative-integer? x)
                                          (exact-nonnegative-integer? y))
                                   (unit (if (< x y) 1 0))
                                   ((oracle '<) x y)))]
                              [u%
                               (λ (x y)
                                 (if (and (exact-nonnegative-integer? x)
                                          (exact-nonnegative-integer? y))
                                   (raise `(% ,x ,y))
                                   (λ (ς)
                                     (displayln 'u%)
                                     (println x)
                                     (println y)
                                     (cons (list (read)) ς))))]
                              [!
                               (λ (x)
                                 (if (exact-nonnegative-integer? x)
                                   (unit (if (zero? x) 1 0))
                                   ((oracle '!) x)))]
                              [&&
                               (λ (x y)
                                 (if (and (exact-nonnegative-integer? x)
                                          (exact-nonnegative-integer? y))
                                   (unit (if (or (zero? x)
                                                 (zero? y))
                                           0
                                           1))
                                   ((oracle '&&) x y)))]
                              [\|\|
                               (λ (x y)
                                 (if (and (exact-nonnegative-integer? x)
                                          (exact-nonnegative-integer? y))
                                   (unit (if (and (zero? x)
                                                  (zero? y))
                                           0
                                           1))
                                   ((oracle '\|\|) x y)))]
                              [app-global-put
                               (λ (k v)
                                 (upd 'global (λ (gbl) (cons (list k v) gbl))))]
                              [app-global-get
                               (λ (k)
                                 (or (and (bytes? k)
                                          (>>= (get 'global)
                                               (letrec ([lookup (match-lambda
                                                                  [(list)
                                                                   (unit `(app-global ,k))]
                                                                  [(cons (list k′ v) glbls)
                                                                   (if (bytes? k′)
                                                                     (if (bytes=? k k′)
                                                                       (unit v)
                                                                       (lookup glbls))
                                                                     ((oracle 'app-global-get) k))])])
                                                 lookup)))
                                     ((oracle 'app-global-get) k)))]
                              [app-global-get-ex
                               (λ (a k)
                                 (λ (ς)
                                   (displayln 'app-global-get-ex)
                                   (println a)
                                   (println k)
                                   (cons (list (read) (read)) ς)))]
                              [store
                               (λ (i x) (upd 'scratchspace (λ (ss) (hash-set ss i x))))]
                              [load
                               (λ (i) (>>= (get 'scratchspace) (λ (ss) (unit (hash-ref ss i 0)))))]
                              [transaction
                               unit]
                              [transaction-array
                               (λ (f ai) (unit (list 'txn-array f ai)))]
                              [group-transaction
                               (λ (gi f) (unit (list 'group-txn gi f)))]
                              [global
                               unit]
                              [btoi
                               (λ (x)
                                 ((oracle 'btoi) x))]
                              [asset-holding-get
                               (λ (a b f) (unit (list a b f) 1))]
                              [logic-sig-version
                               (get 'logic-sig-version)]
                              [read-instruction
                               (>>= (get 'instructions)
                                    (match-lambda
                                      [(cons instr instrs)
                                       (>> ((put 'instructions) instrs)
                                           (unit instr))]
                                      [(list)
                                       fail]))]
                              [check-final
                               (>>= (get 'instructions)
                                    (match-lambda
                                      [(list)
                                       (>>= (get 'stack)
                                            (match-lambda
                                              [(list x)
                                               (>>= (is-zero x)
                                                    (λ (zero?) (return (if zero? 0 1))))]
                                              [_
                                               fail]))]
                                      [_
                                       (unit)]))]
                              [put-intcblock
                               (put 'intcblock)]
                              [get-intcblock
                               (get 'intcblock)]
                              [put-bytecblock
                               (put 'bytecblock)]
                              [get-bytecblock
                               (get 'bytecblock)]
                              [jump
                               (put 'instructions)])))
               'step)])
       (let loop ([ς (hasheq 'instructions cfg
                             'logic-sig-version lsv
                             'stack (list)
                             'intcblock (list)
                             'bytecblock (list)
                             'scratchspace (hasheqv)
                             'global (list)
                             'props (set))])
         (pretty-print (hash-set ς 'instructions (match (hash-ref ς 'instructions)
                                                   [(list* a b c _)
                                                    (list a b c)]
                                                   [(list* a b _)
                                                    (list a b)]
                                                   [(list* a _)
                                                    (list a)]
                                                   [(list* _)
                                                    (list)])))
         (match (→ ς)
           [(cons (list) ς)
            (loop ς)]
           [(? exact-nonnegative-integer? x)
            x])))]))

(provide debug)
