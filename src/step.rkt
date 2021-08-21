#lang racket/base
(require racket/match
         (prefix-in r: racket/set)
         (only-in racket/list append-map)
         "monad.rkt"
         "read-byte.rkt"
         "prefix.rkt"
         "vm.rkt")

(require racket/pretty)

; (: inject (→ (exact-nonnegative-integer? bytes?) State))
(define (inject lsv bs)
  (hasheq 'logic-sig-version lsv
          'bytecode bs
          'pc 0
          'stack (list)
          'intcblock (list)
          'bytecblock (list)
          'scratch-space (hasheqv)
          'path-condition (r:set)))

(struct underway (xs state))
(struct returned (code))
(struct failure! (msg))

; (define-type (Step a) (→ (State) (Result a))

(define Step-Monad
  (Monad (λ xs (λ (st) (list (underway xs st))))
         (λ (m f)
           (λ (st)
             (append-map
              (match-lambda
               [(underway xs st)
                ((apply f xs) st)]
               [r (list r)])
              (m st))))
         (λ (m₀ m₁)
           (λ (st)
             (append-map
              (match-lambda
                [(underway xs st)
                 (m₁ st)]
                [r (list r)])
              (m₀ st))))))

(define ((return x) st)
  (list (returned x)))
(define ((fail! template . args) st)
  (list (failure! (apply format template args))))


; get : key -> Step a
(define-syntax get
  (syntax-rules ()
    [(_ key)
     (λ (st) (list (underway (list (hash-ref st 'key)) st)))]
    [(_ key default)
     (λ (st) (list (underway (list (hash-ref st 'key default)) st)))]))
; set : key val -> Step ()
(define-syntax-rule (set key val)
  (λ (st) (list (underway (list) (hash-set st 'key val)))))
; update : key f -> Step ()
(define-syntax-rule (update key f)
  (λ (st) (list (underway (list) (hash-update st 'key f)))))

(define Step-MonadPlus
  (MonadPlus Step-Monad
             (λ (st) (list))
             (λ (m₀ m₁) (λ (st) (append (m₀ st) (m₁ st))))))

(define Step-ReadByte
  (match-let ([(Monad unit >>= >>) Step-Monad])
    (ReadByte Step-Monad
              (>>= (get bytecode)
                   (λ (bc)
                     (>>= (get pc)
                          (λ (pc)
                            (if (>= pc (bytes-length bc))
                              (fail! "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                              (>> (update pc add1)
                                  (unit (bytes-ref bc pc)))))))))))

; instance VM <hash-thing>
(define Step-VM
  (match-let ([(MonadPlus (Monad unit >>= >>) mzero mplus) Step-MonadPlus])
    (VM Step-MonadPlus Step-ReadByte
        ; fail!
        fail!
        ; logic-sig-version
        (get logic-sig-version)
        ; in-mode
        (λ (target-mode info)
          (>>= (get mode #f)
               (match-lambda
                 [#f
                  (set mode target-mode)]
                 [mode
                  (if (eq? mode target-mode)
                    (unit)
                    (fail! "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))
        ; get-pc
        (get pc)
        ; set-pc
        (λ (pc) (set pc pc))
        ; get-bytecode
        (get bytecode)
        ; get-intcblock
        (get intcblock)
        ; put-intcblock
        (λ (xs) (set intcblock xs))
        ; get-bytecblock
        (get bytecblock)
        ; put-bytecblock
        (λ (bss) (set bytecblock bss))
        ; push
        (λ (x) (update stack (λ (stk) (cons x stk))))
        ; pop
        (>>= (get stack)
             (match-lambda
               [(cons x stk)
                (>> (set stack stk)
                    (unit x))]
               [(list)
                (fail! "tried to pop an empty stack")]))
        ; +
        (λ (x y) (unit `(+ ,x ,y)))
        ; -
        (λ (x y) (unit `(- ,x ,y)))
        ; itob
        (λ (x) (unit `(itob ,x)))
        ; is-zero
        (λ (c)
          (define (verify m c)
            (>>= (get path-condition)
                 (λ (pc)
                   (pretty-print pc)
                   (displayln "---")
                   (pretty-print c)
                   (displayln "Can the above condition hold?")
                   (if (read) m mzero))))
          (mplus (verify (>> (update path-condition (λ (cs) (r:set-add cs `(~ ,c))))
                             (unit #t))
                         `(~ ,c))
                 (verify (>> (update path-condition (λ (cs) (r:set-add cs c)))
                             (unit #f))
                         c)))
        ; &&
        (λ (x y) (unit `(&& ,x ,y)))
        ; ||
        (λ (x y) (unit `(!! ,x ,y)))
        ; ==
        (λ (x y) (unit `(== ,x ,y)))
        ; !
        (match-lambda
          [`(~ ,c) (unit c)]
          [c (unit `(~ ,c))])
        ; concat
        (λ (x y) (unit `(concat ,x ,y)))
        ; transaction
        (match-lambda
          [0  ; Sender
           (unit 'Sender)]
          [1  ; Fee 
           (unit 'Fee)]
          [25 ; OnCompletion
           (unit 'OnCompletion)]
          [27 ; NumAppArgs
           #;
           (>> (logic-sig-version>= 2 "NumAppArgs")
               (unit 'NumAppArgs))
           (unit 'NumAppArgs)])
        ; global
        (match-lambda
          [7 ; LatestTimestamp
           #;
           (>> (logic-sig-version>= 2 "LatestTimestamp")
               (unit 'LatestTimestamp))
           (mplus (fail! "negative LatestTimestamp")
                  (unit 'LatestTimestamp))])
        ; global-transaction
        (λ (ti fi)
          (match fi
            [8  ; Amount
             (unit `(txn ,ti Amount))]
            [18 ; AssetAmount
             (unit `(txn ,ti AssetAmount))]))
        ; transaction-array
        (λ (fi ai)
          (match fi
            [26 ; ApplicationArgs
             #;(logic-sig-version>= 2 "ApplicationArgs")
             (unit `(ApplicationArgs ,ai))]
            [28 ; Accounts
             #;(logic-sig-version>= 2 "Accounts")
             (unit `(Accounts ,ai))]))
        ; load
        (λ (i)
          (>>= (get scratch-space)
               (λ (ss)
                 (unit (hash-ref ss i `(scratch-space ,i))))))
        ; store
        (λ (i x) (update scratch-space (λ (ss) (hash-set ss i x))))
        ; balance
        (λ (acct) (unit `(balance ,acct)))
        ; min-balance
        (λ (acct) (unit `(min-balance ,acct)))
        ; app-local-get
        (λ (aid key) (unit `(app-local-get ,aid ,key)))
        ; asset-holding-get
        (λ (acct asset x)
          (mplus (unit `(asset-holding-get ,acct ,asset ,x))
                 (unit #f)))
        ; check-final
        (>>= (get bytecode)
             (λ (bc)
               (>>= (get pc)
                    (λ (pc)
                      (if (= pc (bytes-length bc))
                        (>>= (get stack)
                             (match-lambda
                               [(list) (fail! "stack is empty at end of program")]
                               [(list x) (return x)]
                               [_ (fail! "stack has more than one value at end of program")]))
                        (unit)))))))))

(module+ main
  (require racket/file
           racket/pretty)
  
  (match (current-command-line-arguments)
    [(vector filename)
     (displayln filename)
     (match ((read-prefix read-ReadByte) (file->bytes filename))
       [(cons lsv bs)
        (let loop ([st (inject lsv bs)])
          (pretty-print (hash-remove st 'bytecode))
          (printf "0x~a\n" (number->string (bytes-ref (hash-ref st 'bytecode) (hash-ref st 'pc))
                                           16))
          (for-each
           (match-lambda
             [(underway (list) st)
              (loop st)]
             [(failure! msg)
              (printf "FAIL: ~a\n" msg)])
           ((step Step-VM) st)))])]))
