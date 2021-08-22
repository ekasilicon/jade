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
          'app-global (list)))

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
    (define (push x) (update stack (λ (stk) (cons x stk))))
    (define pop
      (>>= (get stack)
           (match-lambda
             [(cons x stk)
              (>> (set stack stk)
                  (unit x))]
             [(list)
              (fail! "tried to pop an empty stack")])))
    (VM Step-MonadPlus Step-ReadByte
        ; fail!
        fail!
        ; return!
        return
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
          (mplus (fail! "~v longer than 8 bytes" x)
                 (unit `(btoi ,x))))
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
          (define ((add-path-condition c) pc)
            (let loop ([c c]
                       [pc pc])
              (match c
                [`(∧ ,c₀ ,c₁)
                 (loop c₁ (loop c₀ pc))]
                [`(¬ (∨ ,c₀ ,c₁))
                 (loop `(¬ ,c₁) (loop `(¬ ,c₀) pc))]
                [`(¬ (&& ,c₀ ,c₁))
                 (r:set-add pc `(∨ (¬ ,c₀) (¬ ,c₁)))]
                [c
                 (r:set-add pc c)])))
          (define affirm-path-condition
            (match-lambda
              [`(¬ ,c)
               (negate-path-condition c)]
              [c
               (add-path-condition c)]))
          (define negate-path-condition
            (match-lambda
              [`(¬ ,c)
               (affirm-path-condition c)]
              [c
               (add-path-condition `(¬ ,c))]))
          (define (verify m c)
            (>>= (get path-condition)
                 (λ (pc)
                   (cond
                     [(r:set-empty? pc)
                      ; no constraints yet
                      m]
                     [else
                      m
                      #;
                      (pretty-print pc)
                      #;
                      (displayln "---")
                      #;
                      (pretty-print c)
                      #;
                      (displayln "Can the above condition hold?")
                      #;
                      (if (read) m mzero)]))))
          (mplus (verify (>> (update path-condition (negate-path-condition c))
                             (unit #t))
                         `(¬ ,c))
                 (verify (>> (update path-condition (affirm-path-condition c))
                             (unit #f))
                         c)))
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
          (mplus (fail! "substring3 out of bounds: ~v" `(substring3 ,a ,b ,c))
                 (unit `(substring3 ,a ,b ,c))))
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
               (unit 'NumAppArgs))])
        ; global
        (match-lambda
          [4 ; GroupSize
           (unit 'GroupSize)]
          [6 ; Round
           (>> ((logic-sig-version>= Step-VM) 2 "Round")
               (unit 'Round))]
          [7 ; LatestTimestamp
           (>> ((logic-sig-version>= Step-VM) 2 "LatestTimestamp")
               (unit 'LatestTimestamp))]
          [9 ; CreatorAddress
           ; "Address of the creator of the current application. Fails if no such application is executing."
           (>> ((logic-sig-version>= Step-VM) 3 "CreatorAddress")
               (unit 'CreatorAddress))])
        ; global-transaction
        (λ (ti fi)
          (match fi
            [0  ; Sender
             (unit `(txn ,ti Sender))]
            [7  ; Receiver
             (unit `(txn ,ti Receiver))]
            [8  ; Amount
             (unit `(txn ,ti Amount))]
            [16 ; TypeEnum
             (unit `(txn ,ti TypeEnum))]
            [17 ; XferAsset
             (unit `(txn ,ti XferAsset))]
            [18 ; AssetAmount
             (unit `(txn ,ti AssetAmount))]
            [20 ; AssetReceiver
             (unit `(txn ,ti AssetReceiver))]
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
        (λ (aid key)
          (>>= (get app-local)
               (match-lambda
                 [(list)
                  (unit `(app-local-get ,aid ,key))]
                 [al
                  (match `(app-local-get ,aid ,key)
                    ['(app-local-get 1 (concat (concat Sender #"e") (itob (app-local-get 0 #"a2"))))
                     (unit '(app-local-get 1 (concat (concat Sender #"e") (itob (app-local-get 0 #"a2")))))]
                    ['(app-local-get 1 (concat (concat Sender #"e") (itob (app-local-get 0 #"a1"))))
                     (unit '(app-local-get 1 (concat (concat Sender #"e") (itob (app-local-get 0 #"a1")))))]
                    ['(app-local-get 1 (concat (concat Sender #"e") (itob (app-local-get 0 #"lt"))))
                     (unit '(app-local-get 1 (concat (concat Sender #"e") (itob (app-local-get 0 #"lt")))))]
                    ['(app-local-get 0 #"c2")
                     (unit '(app-local-get 0 #"c2"))]
                    ['(app-local-get 0 #"p2")
                     (unit '(app-local-get 0 #"p2"))]
                    ['(app-local-get 1 (concat (concat Sender #"e") (itob (txn 2 XferAsset))))
                     (unit '(app-local-get 1 (concat (concat Sender #"e") (itob (txn 2 XferAsset)))))]
                    [_
                     (pretty-print al)
                     (pretty-print `(app-local-get ,aid ,key))
                     (failure-cont)])])))
        ; app-local-put
        (λ (acct key val)
          (update app-local (λ (al) (cons `(put ,acct ,key ,val) al))))
        ; app-local-del
        (λ (acct key)
          (update app-local (λ (al) (cons `(del ,acct ,key) al))))
        ; app-global-get
        (λ (key)
          (>>= (get app-global)
               (match-lambda
                 [(list) (unit `(app-global-get ,key))]
                 [ag
                  (match `(app-global-get ,key)
                    ['(app-global-get #"End round")
                     (unit '(app-global-get #"End round"))]
                    ['(app-global-get #"Duration")
                     (unit '(app-global-get #"Duration"))]
                    [_
                     (pretty-print ag)
                     (pretty-print `(app-global-get ,key))
                     (failure-cont)])])))
        ; app-global-put
        (λ (key val)
          (update app-global (λ (ag) (cons `(put ,key ,val) ag))))
        ; asset-holding-get
        (λ (acct asset x)
          (>>= (unit 42)
               (match-lambda
                 [42
                  (>> (push `(asset-holding-get ,acct ,asset ,x))
                      (push `(asset-holding-get-exists ,acct ,asset ,x)))])))
        ; asset-params-get
        (λ (asset x)
          (>>= (unit 42)
               (match-lambda
                 [42
                  (>> (push `(asset-params-get ,asset ,x))
                      (push `(asset-params-get-exists ,asset ,x)))])))
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
