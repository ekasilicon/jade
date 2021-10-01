#lang racket/base
(require racket/match
         "record.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "instruction.rkt")

(record VM (MonadPlus
            ReadByte
            panic
            return!
            logic-sig-version
            in-mode
            get-pc
            set-pc
            get-bytecode
            get-intcblock
            put-intcblock
            get-bytecblock
            put-bytecblock
            push
            pop
            push-call
            pop-call
            sha256
            +
            -
            /
            *
            len
            itob
            btoi
            %
            mulw
            addw
            divmodw
            is-zero
            &&
            \|\|
            =
            <
            !
            ~
            concat
            substring
            getbyte
            global
            transaction
            group-transaction
            transaction-array
            group-transaction-array
            load
            store
            balance
            min-balance
            app-local-get
            app-local-put
            app-local-del
            app-local-get-ex
            app-global-get
            app-global-put
            app-global-get-ex
            asset-holding-get
            asset-params-get
            bzero
            check-final))

; execute : VM m s => Instruction → m ()
(define (execute vm)
  (match-define (VM [MonadPlus (MonadPlus [Monad (Monad unit >>= >>)])]
                    [ReadByte rb]
                    panic
                    return!
                    logic-sig-version
                    in-mode
                    get-pc
                    set-pc
                    get-bytecode
                    get-intcblock
                    put-intcblock
                    get-bytecblock
                    put-bytecblock
                    push
                    pop
                    push-call
                    pop-call
                    sha256
                    [+ vm+]
                    [- vm-]
                    [/ vm/]
                    [* vm*]
                    len
                    itob
                    btoi
                    %
                    mulw
                    addw
                    divmodw
                    is-zero
                    &&
                    \|\|
                    [= vm=]
                    [< vm<]
                    [! vm!]
                    [~ vm~]
                    concat
                    substring
                    getbyte
                    global
                    transaction
                    group-transaction
                    transaction-array
                    group-transaction-array
                    load
                    store
                    balance
                    min-balance
                    app-local-get
                    app-local-put
                    app-local-del
                    app-local-get-ex
                    app-global-get
                    app-global-put
                    app-global-get-ex
                    asset-holding-get
                    asset-params-get
                    bzero)
    vm)
  (define lsv>= (logic-sig-version>= vm))
  (define (lookup-intcblock i)
    (>>= get-intcblock
         (λ (xs)
           (if (< i (length xs))
             (unit (list-ref xs i))
             (panic "intcblock has ~a ints but index ~a requested" (length xs) i)))))
  (define (lookup-bytecblock i)
    (>>= get-bytecblock
         (λ (bss)
           (if (< i (length bss))
             (unit (list-ref bss i))
             (panic "bytecblock has ~a bytes but index ~a requested" (length bss) i)))))
  (define continue (unit (list)))
  (define (jump offset)
    (>>= logic-sig-version
         (λ (lsv)
           (if (and (< offset 0)
                    (< lsv 4))
             (panic "cannot jump backwards (offset ~a) in LogicSig version ~a < 4" offset lsv)
             (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
  (define (goto pc)
    (if (< pc 0)
      (panic "cannot go to negative counter ~a" pc)
      (>>= get-bytecode
           (λ (bc)
             (if (> pc (bytes-length bc))
               (panic "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
               (>>= logic-sig-version
                    (λ (lsv)
                      (if (and (= pc (bytes-length bc))
                               (< lsv 2))
                        (panic "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                        (set-pc pc)))))))))
  (define (stack-apply f arity)
    (>>= (>>= (let loop ([n arity]
                         [xs (list)])
                (if (zero? n)
                  (apply unit xs)
                  (>>= pop (λ (x) (loop (sub1 n) (cons x xs))))))
              f)
         push))
  (define swap
    (>>= pop
         (λ (b)
           (>>= pop
                (λ (a)
                  (>> (push b)
                      (push a)))))))
  (match-lambda
    [`(err)
     (panic "err")]
    [`(sha256)
     (stack-apply sha256 1)]
    [`(+)
     (stack-apply vm+ 2)]
    [`(-)
     (stack-apply vm- 2)]
    [`(/)
     (>>= pop
          (λ (b)
            (>>= pop
                 (λ (a)
                   (>>= (is-zero b)
                        (λ (zero?)
                          (if zero?
                            (panic "/ by 0")
                            (>>= (vm/ a b) push))))))))]
    [`(*)
     (stack-apply vm* 2)]
    [`(<)
     (stack-apply vm< 2)]
    [`(>)
     (>> swap
         (stack-apply vm< 2))]
    [`(<=)
     (>> swap
         (stack-apply vm< 2)
         (stack-apply vm! 1))]
    [`(>=)
     (>> (stack-apply vm< 2)
         (stack-apply vm! 1))]
    [`(&&)
     (stack-apply && 2)]
    [`(\|\|)
     (stack-apply \|\| 2)]
    [`(==)
     (stack-apply vm= 2)]
    [`(!=)
     (>> (stack-apply vm= 2)
         (stack-apply vm! 1))]
    [`(!)
     (stack-apply vm! 1)]
    [`(len)
     (stack-apply len 1)]
    [`(itob)
     (stack-apply itob 1)]
    [`(btoi)
     (stack-apply btoi 1)]
    [`(%)
     (>>= pop
          (λ (b)
            (>>= pop
                 (λ (a)
                   (>>= (is-zero b)
                        (λ (zero?)
                          (if zero?
                            (panic "% by 0")
                            (>>= (% a b) push))))))))]
    [`(~)
     (stack-apply vm~ 1)]
    [`(mulw)
     (>>= pop (λ (b) (>>= pop (λ (a) (mulw a b)))))]
    [`(addw)
     (>>= pop (λ (b) (>>= pop (λ (a) (addw a b)))))]
    [`(divmodw)
     (>>= pop (λ (d) (>>= pop (λ (c) (>>= pop (λ (b) (>>= pop (λ (a) (divmodw a b c d)))))))))]
    [`(intcblock . ,ns)
     (put-intcblock ns)]
    [`(intc ,i)
     (>>= (lookup-intcblock i) push)]
    [`(intc_0)
     (>>= (lookup-intcblock 0) push)]
    [`(intc_1)
     (>>= (lookup-intcblock 1) push)]
    [`(intc_2)
     (>>= (lookup-intcblock 2) push)]
    [`(intc_3)
     (>>= (lookup-intcblock 3) push)]
    [`(bytecblock . ,bss)
     (put-bytecblock bss)]
    [`(bytec ,i)
     (>>= (lookup-bytecblock i) push)]
    [`(bytec_0)
     (>>= (lookup-bytecblock 0) push)]
    [`(bytec_1)
     (>>= (lookup-bytecblock 1) push)]
    [`(bytec_2)
     (>>= (lookup-bytecblock 2) push)]
    [`(bytec_3)
     (>>= (lookup-bytecblock 3) push)]
    [`(txn ,f)
     (>>= (transaction f) push)]
    [`(global ,f)
     (>>= (global f) push)]
    [`(gtxn ,gi ,f)
     (>>= (group-transaction gi f) push)]
    [`(load ,i)
     (>>= (load i) push)]
    [`(store ,i)
     (>>= pop (λ (x) (store i x)))]
    [`(txna ,f ,ai)
     (>> (lsv>= 2 "txna")
         (>>= (transaction-array f ai) push))]
    [`(gtxna ,gi ,f ,ai)
     (>>= (group-transaction-array gi f ai) push)]
    [`(gtxns ,f)
     (>> (lsv>= 3 "gtxns")
         (>>= (>>= pop (λ (gi) (group-transaction gi f))) push))]
    [`(bnz ,offset)
     (>>= (>>= pop is-zero)
          (λ (stay?)
            (if stay?
              continue
              (jump offset))))]
    [`(bz ,offset)
     (>>= (>>= pop is-zero)
          (λ (jump?)
            (if jump?
              (jump offset)
              continue)))]
    [`(b ,offset)
     (>> (lsv>= 2 "b")
         (jump offset))]
    [`(return)
     (>> (lsv>= 2 "return")
         (>>= pop return!))]
    [`(assert)
     (>> (lsv>= 3 "assert")
         (>>= pop
              (λ (x)
                (>>= (is-zero x)
                     (λ (fail?)
                       (if fail?
                         (panic "assert: ~v" x)
                         continue))))))]
    [`(pop)
     (>> pop
         continue)]
    [`(dup)
     (>>= pop
          (λ (x)
            (>> (push x)
                (push x))))]
    [`(dup2)
     (>> (lsv>= 2 "dup2")
         (>>= pop
              (λ (b)
                (>>= pop
                     (λ (a)
                       (>> (push a)
                           (push b)
                           (push a)
                           (push b)))))))]
    [`(dig ,n)
     (>> (lsv>= 3 "dig")
         (>>= (let loop ([n n])
                (>>= pop
                     (λ (x)
                       (if (zero? n)
                         (>> (push x)
                             (unit x))
                         (>>= (loop (sub1 n))
                              (λ (y)
                                (push x)
                                (unit y)))))))
              push))]
    [`(swap)
     (>> (lsv>= 3 "swap")
         swap)]
    [`(select)
     (>> (lsv>= 3 "select")
         (>>= pop
              (λ (c)
                (>>= pop
                     (λ (b)
                       (>>= pop
                            (λ (a)
                              (>>= (is-zero c)
                                   (λ (zero?) (push (if zero? a b)))))))))))]
    [`(concat)
     (>> (lsv>= 2 "concat")
         (stack-apply concat 2))]
    [`(substring ,s ,e)
     (>> (lsv>= 2 "substring")
         (>>= (>>= pop (λ (a) (substring a s e)))
              push))]
    [`(substring3)
     (>> (lsv>= 2 "substring3")
         (stack-apply substring 3))]
    [`(getbyte)
     (>> (lsv>= 3 "getbyte")
         (stack-apply getbyte 2))]
    [`(balance)
     (>> (lsv>= 2 "balance")
         (in-mode 'Application "balance")
         (stack-apply balance 1))]
    [`(app_local_get)
     (>> (lsv>= 2 "app_local_get")
         (in-mode 'Application "app_local_get")
         (stack-apply app-local-get 2))]
    [`(app_local_get_ex)
     (>> (lsv>= 2 "app_local_get_ex")
         (in-mode 'Application "app_local_get_ex")
         (>>= pop (λ (c) (>>= pop (λ (b) (>>= pop (λ (a) (app-local-get-ex a b c))))))))]
    [`(app_global_get)
     (>> (lsv>= 2 "app_global_get")
         (in-mode 'Application "app_global_get")
         (stack-apply app-global-get 1))]
    [`(app_global_get_ex)
     (>> (lsv>= 2 "app_global_get_ex")
         (in-mode 'Application "app_global_get_ex")
         (>>= pop (λ (b) (>>= pop (λ (a) (app-global-get-ex a b))))))]
    [`(app_local_put)
     (>> (lsv>= 2 "app_local_put")
         (in-mode 'Application "app_local_put")
         (>>= pop (λ (c) (>>= pop (λ (b) (>>= pop (λ (a) (app-local-put a b c))))))))]
    [`(app_global_put)
     (>> (lsv>= 2 "app_global_put")
         (in-mode 'Application "app_global_put")
         (>>= pop (λ (b) (>>= pop (λ (a) (app-global-put a b))))))]
    [`(app_local_del)
     (>> (lsv>= 2 "app_local_del")
         (in-mode 'Application "app_local_del")
         (>>= pop (λ (b) (>>= pop (λ (a) (app-local-del a b))))))]
    [`(asset_holding_get)
     (>> (lsv>= 2 "asset_holding_get")
         (in-mode 'Application "asset_holding_get")
         (>>= (read-uint8 rb)
              (λ (fi) (>>= pop (λ (b) (>>= pop (λ (a) (asset-holding-get a b fi))))))))]
    [`(asset_params_get)
     (>> (lsv>= 2 "asset_params_get")
         (in-mode 'Application "asset_params_get")
         (>>= (read-uint8 rb)
              (λ (fi) (>>= pop (λ (a) (asset-params-get a fi))))))]
    [`(min_balance)
     (>> (lsv>= 3 "min_balance")
         (in-mode 'Application "min_balance")
         (stack-apply min-balance 1))]
    [`(pushbytes ,bs)
     (>> (lsv>= 3 "pushbytes")
         (push bs))]
    [`(pushint ,n)
     (>> (lsv>= 3 "pushint")
         (push n))]
    [`(callsub ,offset)
     (>> (lsv>= 4 "callsub")
         (>>= get-pc
              (λ (ret-pc)
                (>> (push-call ret-pc)
                    (jump offset)))))]
    [`(retsub)
     (>> (lsv>= 4 "retsub")
         (>>= pop-call goto))]
    [`(bzero)
     (>> (lsv>= 4 "bzero")
         (stack-apply bzero 1))]
    [bc
     (display "0x")
     (displayln (number->string bc 16))
     (failure-cont)]))

; step : VM m s => m ()
(define (step vm)
  (match-define (VM [MonadPlus (MonadPlus [Monad (Monad >>= >>)])]
                    [ReadByte rb]
                    check-final)
    vm)
  (>> (>>= (read-instruction rb)
           (execute vm))
      check-final))

(define (logic-sig-version>= vm)
  (match-define (VM [MonadPlus (MonadPlus [Monad (Monad unit >>=)])]
                    logic-sig-version
                    panic)
    vm)
  (λ (target-lsv info)
    (>>= logic-sig-version
         (λ (lsv)
           (if (>= lsv target-lsv)
             (unit)
             (panic "LogicSig version = ~a but need >= ~a for ~a" lsv target-lsv info))))))

(provide VM
         step
         logic-sig-version>=)
