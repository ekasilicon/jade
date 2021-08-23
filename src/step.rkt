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
          'path-condition (r:set)
          'app-local (list) ; at best a map, at worst a sequence of puts
          'app-global (list)
          'execution-log (list)))

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
(define ((panic template . args) st)
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
(define-syntax update
  (syntax-rules ()
    [(_ key f)
     (λ (st) (list (underway (list) (hash-update st 'key f))))]
    [(_ key f iv)
     (λ (st) (list (underway (list) (hash-update st 'key f iv))))]))

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
                              (panic "attempt to read at ~a but bytecode ends at ~a" pc (bytes-length bc))
                              (>> (update pc add1)
                                  (unit (bytes-ref bc pc)))))))))))

; instance VM <hash-thing>
(define Step-VM
  (match-let ([(MonadPlus (Monad unit >>= >>) mzero mplus) Step-MonadPlus])
    (define (interpretation #:assume assume #:reject reject)
      (match-lambda
        [`(¬ ,c) (reject c)]
        [c       (assume c)]))
    (define interpretations
      (list
       #;
       (interpretation
        #:assume
        (match-lambda
          [`(= OnCompletion ,oc)
           (>>= (get OnCompletion #f)
                (match-lambda
                  [#f
                   (set OnCompletion `(known ,oc))]
                  [`(known ,oc₀)
                   (assume `(= ,oc₀ ,oc))]
                  [`(eliminated ,ocs)
                   (for/fold ([m (set OnCompletion `(known ,oc))])
                             ([oc₀ (in-set ocs)])
                     (>> (reject `(= ,oc ,oc₀))
                         m))]))]
          [_ #f])
        #:reject
        (match-lambda
             [`(= OnCompletion ,oc)
              (>>= (get OnCompletion #f)
                   (match-lambda
                     [#f
                      (set OnCompletion `(eliminated ,(r:set oc)))]
                     [`(known ,oc₀)
                      (reject `(= ,oc₀ ,oc))]
                     [`(eliminated ,ocs)
                      (set OnCompletion `(eliminated ,(r:set-add ocs oc)))]))
              (update OnCompletion
                      (match-lambda
                        [`(known ,oc₀)
                         `(known ,oc₀)]
                        [`(eliminated ,oc₀)
                         (let* ([oc₀ (r:set-add oc₀ oc)]
                                [occ (for/fold ([ocs (r:set 0 1 2 4 5)])
                                               ([oc (r:in-set oc₀)])
                                       (r:set-remove ocs oc))])
                           (if (= (r:set-count occ) 1)
                             `(known ,(r:set-first occ))
                             `(eliminated ,oc₀)))])
                      `(eliminated ,(r:set)))]
             [_ #f]))
       (interpretation
        #:assume
        (match-lambda
          [`(< ,x ,x)
           mzero]
          [_ #f])
        #:reject
        (match-lambda
          [`(< ,x ,x)
           (unit)]
          [_ #f]))
       #;
       (interpretation
        #:assume
        (match-lambda
          [`(= (ApplicationArgs))]))))
    
    (define (push x) (update stack (λ (stk) (cons x stk))))
    (define pop
      (>>= (get stack)
           (match-lambda
             [(cons x stk)
              (>> (set stack stk)
                  (unit x))]
             [(list)
              (panic "tried to pop an empty stack")])))
    (define assume
      (match-lambda
        [`(¬ ,c)
         (reject c)]
        [`(∧ ,c₀ ,c₁)
         (>> (assume c₀)
             (assume c₁))]
        [`(∨ ,c₀ ,c₁)
         (mplus (assume c₀)
                (assume c₁))]
        [(? exact-nonnegative-integer? n)
         (if (zero? n)
           mzero
           (unit))]
        [`(= ,x₀ ,x₁)
         (cond
           [(and (bytes? x₀)
                 (bytes? x₁))
            (if (bytes=? x₀ x₁)
              (unit)
              mzero)]
           [(and (exact-nonnegative-integer? x₀)
                 (exact-nonnegative-integer? x₁))
            (if (= x₀ x₁)
              (unit)
              mzero)]
           [(equal? x₀ x₁)
            (unit)]
           [else
            (failure-cont)])]
        [c
         (or (ormap (λ (i) (i c)) interpretations)
             (failure-cont))]
        [c
         (>>= (get path-condition)
              (λ (pc)
                (if (r:set-member? pc `(¬ ,c))
                  mzero
                  (update path-condition (λ (pc) (r:set-add pc c))))))]))
    (define reject
      (match-lambda
        [`(¬ ,c)
         (assume c)]
        [`(∨ ,c₀ ,c₁)
         (>> (reject c₀)
             (reject c₁))]
        [`(∧ ,c₀ ,c₁)
         (mplus (reject c₀)
                (reject c₁))]
        [(? exact-nonnegative-integer? n)
         (if (zero? n)
           (unit)
           mzero)]
        [`(= ,x₀ ,x₁)
         (cond
           [(and (bytes? x₀)
                 (bytes? x₁))
            (if (bytes=? x₀ x₁)
              mzero
              (unit))]
           [(and (exact-nonnegative-integer? x₀)
                 (exact-nonnegative-integer? x₁))
            (if (= x₀ x₁)
              mzero
              (unit))]
           [(equal? x₀ x₁)
            mzero]
           [else
            (failure-cont)])]
        [c
         (or (ormap (λ (i) (i `(¬ ,c))) interpretations)
             (failure-cont))]
        [c
         (>>= (get path-condition)
              (λ (pc)
                (if (r:set-member? pc c)
                  mzero
                  (update path-condition (λ (pc) (r:set-add pc `(¬ ,c)))))))]))
    (define (log msg)
      (update execution-log (λ (msgs) (cons msg msgs))))
    (define <-reduce
      (match-lambda
        [`(¬ ,c)
         `(¬ ,(<-reduce c))]
        [`(∧ ,c₀ ,c₁)
         `(∧ ,(<-reduce c₀)
             ,(<-reduce c₁))]
        [`(∨ ,c₀ ,c₁)
         `(∨ ,(<-reduce c₀)
             ,(<-reduce c₁))]
        [`(> ,x ,y)
         `(< ,y ,x)]
        [`(<= ,x ,y)
         (<-reduce `(>= ,y ,x))]
        [`(>= ,x ,y)
         `(¬ (< ,x ,y))]
        [c c]))
    (VM Step-MonadPlus Step-ReadByte
        ; panic
        panic
        ; return!
        return
        ; logic-sig-version
        (get logic-sig-version)
        ; in-mode
        ; could (assume `(= ApplicationMode ,target-mode)) and work the metalogic out with a custom interpretation
        (λ (target-mode info)
          (>>= (get mode #f)
               (match-lambda
                 [#f
                  (set mode target-mode)]
                 [mode
                  (if (eq? mode target-mode)
                    (unit)
                    (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))
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
        push
        ; pop
        pop
        ; +
        (λ (x y) (unit `(+ ,x ,y)))
        ; -
        (λ (x y) (unit `(- ,x ,y)))
        ; /
        (λ (x y) (unit `(/ ,x ,y)))
        ; *
        (λ (x y) (unit `(* ,x ,y)))
        ; len
        (λ (x) (unit `(len ,x)))
        ; itob
        (λ (x) (unit `(itob ,x)))
        ; btoi
        (λ (x)
          (mplus (>> (assume (<-reduce `(<= (len ,x) 8)))
                     (unit `(btoi ,x)))
                 (>> (reject (<-reduce `(<= (len ,x) 8)))
                     (panic "btoi: input greater than 8 bytes"))))
        ; %
        (λ (x y) (unit `(% ,x ,y)))
        ; mulw
        (λ (a b)
          (>> (push `(hi (mulw ,a ,b)))
              (push `(lo (mulw ,a ,b)))))
        ; addw
        (λ (a b)
          (>> (push `(hi (addw ,a ,b)))
              (push `(lo (addw ,a ,b)))))
        ; divmodw
        (λ (a b c d)
          (let ([dend (match* (a b)
                        [(`(hi ,x) `(lo ,x)) x]
                        [(x y) `(uint128 ,x ,y)])]
                [isor (match* (c d)
                        [(`(hi ,x) `(lo ,x)) x]
                        [(x y) `(uint128 ,x ,y)])])
            (>> (push `(hi (quotient ,dend ,isor)))
                (>> (push `(lo (quotient ,dend ,isor)))
                    (>> (push `(hi (remainder ,dend ,isor)))
                        (push `(lo (remainder ,dend ,isor))))))))
        ; is-zero
        (λ (c)
          (mplus (>> (assume c)
                     (unit #f))
                 (>> (reject c)
                     (unit #t))))
        ; &&
        (λ (x y) (unit `(∧ ,x ,y)))
        ; ||
        (λ (x y) (unit `(∨ ,x ,y)))
        ; =
        (λ (x y) (unit `(= ,x ,y)))
        ; <
        (λ (x y) (unit `(< ,x ,y)))
        ; !
        (match-lambda
          [`(¬ ,c) (unit c)]
          [c (unit `(¬ ,c))])
        ; ~
        (λ (x) (unit `(~ ,x)))
        ; concat
        (λ (x y) (unit `(concat ,x ,y)))
        ; substring3
        (λ (a b c)
          (mplus (>> (assume (<-reduce `(∧ (<= ,b ,c)
                                           (<= ,c (len ,a)))))
                     (unit `(substring3 ,a ,b ,c)))
                 (>> (reject (<-reduce `(∧ (<= ,b ,c)
                                           (<= ,c (len ,a)))))
                     (panic "substring3 out of bounds: ~v" `(substring3 ,a ,b ,c)))))
        ; transaction
        (match-lambda
          [0  ; Sender
           (unit 'Sender)]
          [1  ; Fee 
           (unit 'Fee)]
          [16 ; TypeEnum
           (unit 'TypeEnum)]
          [24 ; ApplicationID
           (>> ((logic-sig-version>= Step-VM) 2 "ApplicationID")
               (unit 'ApplicationID))]
          [25 ; OnCompletion
           (unit 'OnCompletion)]
          [27 ; NumAppArgs
           (>> ((logic-sig-version>= Step-VM) 2 "NumAppArgs")
               (unit 'NumAppArgs))]
          [32 ; 
           (>> ((logic-sig-version>= Step-VM) 2 "NumAppArgs")
               (unit 'RekeyTo))])
        ; global
        (match-lambda
          [3 ; ZeroAddress
           (>> (log "The ZeroAddress symbolic constant represents a single value. Make sure the theories respect it.")
               (unit 'ZeroAddress))]
          [4 ; GroupSize
           (unit 'GroupSize)]
          [6 ; Round
           (>> ((logic-sig-version>= Step-VM) 2 "Round")
               (unit 'Round))]
          [7 ; LatestTimestamp
           (>> ((logic-sig-version>= Step-VM) 2 "LatestTimestamp")
               (>> (log "LatestTimestamp fails if the timestamp given is less than zero. This is neither under the control of the program, nor tracked by the machine.")
                   (unit 'LatestTimestamp)))]
          [9 ; CreatorAddress
           ; "Address of the creator of the current application. Fails if no such application is executing."
           (>> ((logic-sig-version>= Step-VM) 3 "CreatorAddress")
               (unit 'CreatorAddress))])
        ; global-transaction
        (λ (ti fi)
          (match fi
            [0  ; Sender
             (unit `(txn ,ti Sender))]
            [1  ; Fee
             (unit `(txn ,ti Fee))]
            [7  ; Receiver
             (unit `(txn ,ti Receiver))]
            [8  ; Amount
             (unit `(txn ,ti Amount))]
            [9  ; CloseRemainderTo
             (unit `(txn ,ti CloseRemainderTo))]
            [16 ; TypeEnum
             (unit `(txn ,ti TypeEnum))]
            [17 ; XferAsset
             (unit `(txn ,ti XferAsset))]
            [18 ; AssetAmount
             (unit `(txn ,ti AssetAmount))]
            [19 ; AssetSender
             (unit `(txn ,ti AssetSender))]
            [20 ; AssetReceiver
             (unit `(txn ,ti AssetReceiver))]
            [21 ; AssetCloseTo
             (unit `(txn ,ti AssetCloseTo))]
            [38 ; ConfigAssetName
             (>> ((logic-sig-version>= Step-VM) 2 "ConfigAssetName")
                 (unit 'ConfigAssetName))]))
        ; transaction-array
        (λ (fi ai)
          (match fi
            [26 ; ApplicationArgs
             (>> ((logic-sig-version>= Step-VM) 2 "ApplicationArgs")
                 (unit `(ApplicationArgs ,ai)))]
            [28 ; Accounts
             (>> ((logic-sig-version>= Step-VM) 2 "Accounts")
                 (unit `(Accounts ,ai)))]))
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
        (λ (acct key)
          (>>= (get app-local)
               (letrec ([lookup (match-lambda
                                  [(list)
                                   (unit `(app-local-get ,acct ,key))]
                                  [(cons `(put ,put-acct ,put-key ,val)
                                         al)
                                   (mplus (>> (assume `(∧ (= ,acct ,put-acct)
                                                          (= ,key  ,put-key)))
                                              (unit val))
                                          (>> (reject `(∧ (= ,acct ,put-acct)
                                                          (= ,key  ,put-key)))
                                              (lookup al)))])])
                 lookup)))
        ; app-local-put
        (λ (acct key val)
          (update app-local (λ (al) (cons `(put ,acct ,key ,val) al))))
        ; app-local-del
        (λ (acct key)
          (update app-local (λ (al) (cons `(del ,acct ,key) al))))
        ; app-global-get
        (λ (key)
          (>>= (get app-global)
               (letrec ([lookup (match-lambda
                                  [(list)
                                   (unit `(app-global-get ,key))]
                                  [(cons `(put ,put-key ,val)
                                         ag)
                                   (mplus (>> (assume `(= ,key ,put-key))
                                              (unit val))
                                          (>> (reject `(= ,key ,put-key))
                                              (lookup ag)))])])
                 lookup)))
        ; app-global-put
        (λ (key val)
          (update app-global (λ (ag) (cons `(put ,key ,val) ag))))
        ; app-global-get-ex
        (λ (app-id key)
          (>> (push `(app-global ,app-id ,key))
              (push `(app-global-exists ,app-id ,key))))
        ; asset-holding-get
        (λ (acct asset x)
          (>>= (match x
                 [0 (unit 'AssetBalance)]
                 [1 (unit 'AssetFrozen)])
               (λ (f)
                 (>> (push `(asset-holding-get ,acct ,asset ,f))
                     (push `(asset-holding-get-exists ,acct ,asset ,f))))))
        ; asset-params-get
        (λ (asset x)
          (>>= (match x
                 [3 (unit 'AssetUnitName)])
               (λ (f)
                 (>> (push `(asset-params-get ,asset ,f))
                     (push `(asset-params-get-exists ,asset ,f))))))
        ; check-final
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
          #;(pretty-print (hash-remove st 'bytecode))
          (printf "~a: 0x~a\n"
                  (hash-ref st 'pc)
                  (number->string (bytes-ref (hash-ref st 'bytecode) (hash-ref st 'pc))
                                  16))
          (for-each
           (match-lambda
             [(underway (list) st)
              (loop st)]
             [(failure! msg)
              (pretty-print (hash-remove st 'bytecode))
              (printf "FAIL: ~a\n" msg)]
             [(returned code)
              (pretty-print (hash-remove st 'bytecode))
              (printf "RETURN: ~v\n" code)])
           ((step Step-VM) st)))])]))

#;
(= (hi (mulw (+ (+ (txn 4 AssetAmount) 1000) 1)
             (+ (+ (txn 4 AssetAmount) 1000) 1)))
   (hi (mulw (+ (txn 2 AssetAmount) (txn 2 Amount))
             (+ (txn 3 AssetAmount) (txn 3 Amount)))))

#;
(< (lo (mulw (+ (txn 2 AssetAmount) (txn 2 Amount))
             (+ (txn 3 AssetAmount) (txn 3 Amount))))
   (lo (mulw (+ (+ (txn 4 AssetAmount) 1000) 1)
             (+ (+ (txn 4 AssetAmount) 1000) 1))))
