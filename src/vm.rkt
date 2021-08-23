#lang racket/base
(require racket/match
         "monad.rkt"
         "read-byte.rkt")

(struct VM (MonadPlus ReadByte
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
                      !!
                      =
                      <
                      !
                      ~
                      concat
                      substring3
                      transaction
                      global
                      global-transaction
                      transaction-array
                      load
                      store
                      balance
                      min-balance
                      app-local-get
                      app-local-put
                      app-local-del
                      app-global-get
                      app-global-put
                      app-global-get-ex
                      asset-holding-get
                      asset-params-get
                      check-final))

; execute : VM m s => byte → m ()
(define (execute vm)
  (match-define (VM (MonadPlus (Monad unit >>= >>) _ _) rb
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
                    vm+
                    vm-
                    vm/
                    vm*
                    len
                    itob
                    btoi
                    %
                    mulw
                    addw
                    divmodw
                    is-zero
                    &&
                    !!
                    vm=
                    vm<
                    vm!
                    vm~
                    concat
                    substring3
                    transaction
                    global
                    global-transaction
                    transaction-array
                    load
                    store
                    balance
                    min-balance
                    app-local-get
                    app-local-put
                    app-local-del
                    app-global-get
                    app-global-put
                    app-global-get-ex
                    asset-holding-get
                    asset-params-get
                    _ ; check-final
                    ) vm)
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
    [#x00 ; err
     (panic "err")]
    [#x08 ; +
     (stack-apply vm+ 2)]
    [#x09 ; -
     (stack-apply vm- 2)]
    [#x0a ; /
     (>>= pop
          (λ (b)
            (>>= pop
                 (λ (a)
                   (>>= (is-zero b)
                        (λ (zero?)
                          (if zero?
                            (panic "/ by 0")
                            (>>= (vm/ a b) push))))))))]
    [#x0b ; *
     (stack-apply vm* 2)]
    [#x0c ; <
     (stack-apply vm< 2)]
    [#x0d ; >
     (>> swap
         (stack-apply vm< 2))]
    [#x0e ; <=
     (>> (>> swap
             (stack-apply vm< 2))
         (stack-apply vm! 1))]
    [#x0f ; >=
     (>> (stack-apply vm< 2)
         (stack-apply vm! 1))]
    [#x10 ; &&
     (stack-apply && 2)]
    [#x11 ; ||
     (stack-apply !! 2)]
    [#x12 ; ==
     (stack-apply vm= 2)]
    [#x13 ; !=
     (>> (stack-apply vm= 2)
         (stack-apply vm! 1))]
    [#x14 ; !
     (stack-apply vm! 1)]
    [#x15 ; len
     (stack-apply len 1)]
    [#x16 ; itob
     (stack-apply itob 1)]
    [#x17 ; btoi
     (stack-apply btoi 1)]
    [#x18 ; %
     (>>= pop
          (λ (b)
            (>>= pop
                 (λ (a)
                   (>>= (is-zero b)
                        (λ (zero?)
                          (if zero?
                            (panic "% by 0")
                            (>>= (% a b) push))))))))]
    [#x1c ; ~
     (stack-apply vm~ 1)]
    [#x1d ; mulw
     (>>= pop (λ (b) (>>= pop (λ (a) (mulw a b)))))]
    [#x1e ; addw
     (>>= pop (λ (b) (>>= pop (λ (a) (addw a b)))))]
    [#x1f ; divmodw
     (>>= pop (λ (d) (>>= pop (λ (c) (>>= pop (λ (b) (>>= pop (λ (a) (divmodw a b c d)))))))))]
    [#x20 ; intcblock
     (>>= (>>= (read-varuint rb)
               (λ (n)
                 (let loop ([n n])
                   (if (zero? n)
                     (unit (list))
                     (>>= (read-varuint rb)
                          (λ (x)
                            (>>= (loop (sub1 n))
                                 (λ (xs) (unit (cons x xs))))))))))
          put-intcblock)]
    [#x21 ; intc
     (>>= (>>= (read-uint8 rb) lookup-intcblock) push)]
    [#x22 ; intc_0
     (>>= (lookup-intcblock 0) push)]
    [#x23 ; intc_1
     (>>= (lookup-intcblock 1) push)]
    [#x24 ; intc_2
     (>>= (lookup-intcblock 2) push)]
    [#x25 ; intc_3
     (>>= (lookup-intcblock 3) push)]
    [#x26 ; bytecblock
     (>>= (>>= (read-varuint rb)
               (λ (n)
                 (let loop ([n n])
                   (if (zero? n)
                     (unit (list))
                     (>>= (read-bytes rb)
                          (λ (bs)
                            (>>= (loop (sub1 n))
                                 (λ (bss) (unit (cons bs bss))))))))))
          put-bytecblock)]
    [#x27 ; bytec
     (>>= (>>= (read-uint8 rb) lookup-bytecblock) push)]
    [#x28 ; bytec_0
     (>>= (lookup-bytecblock 0) push)]
    [#x29 ; bytec_1
     (>>= (lookup-bytecblock 1) push)]
    [#x2a ; bytec_2
     (>>= (lookup-bytecblock 2) push)]
    [#x2b ; bytec_3
     (>>= (lookup-bytecblock 3) push)]
    [#x31 ; txn
     (>>= (>>= (read-uint8 rb) transaction) push)]
    [#x32 ; global
     (>>= (>>= (read-uint8 rb) global) push)]
    [#x33 ; gtxn
     (>>= (>>= (read-uint8 rb)
               (λ (ti)
                 (>>= (read-uint8 rb)
                      (λ (fi)
                        (global-transaction ti fi)))))
          push)]
    [#x34 ; load
     (>>= (>>= (read-uint8 rb) load) push)]
    [#x35 ; store
     (>>= (read-uint8 rb) (λ (i) (>>= pop (λ (x) (store i x)))))]
    [#x36 ; txna
     (>>= (>>= (read-uint8 rb)
          (λ (fi)
            (>>= (read-uint8 rb)
                 (λ (ai) (transaction-array fi ai)))))
          push)]
    [#x40 ; bnz
     (>>= (read-int16 rb)
          (λ (offset)
            (>>= (>>= pop is-zero)
                 (λ (stay?)
                   (if stay?
                     continue
                     (jump offset))))))]
    [#x41 ; bz
     (>>= (read-int16 rb)
          (λ (offset)
            (>>= (>>= pop is-zero)
                 (λ (jump?)
                   (if jump?
                     (jump offset)
                     continue)))))]
    [#x42 ; b
     (>> (lsv>= 2 "b")
         (>>= (read-int16 rb) jump))]
    [#x43 ; return
     (>> (lsv>= 2 "return")
         (>>= pop return!))]
    [#x44 ; assert
     (>> (lsv>= 3 "assert")
         (>>= pop
              (λ (x)
                (>>= (is-zero x)
                     (λ (fail?)
                       (if fail?
                         (panic "assert: ~v" x)
                         continue))))))]
    [#x48 ; pop
     (>> pop
         continue)]
    [#x49 ; dup
     (>>= pop
          (λ (x)
            (>> (push x)
                (push x))))]
    [#x4a ; dup2
     (>> (lsv>= 2 "dup2")
         (>>= pop
              (λ (b)
                (>>= pop
                     (λ (a)
                       (>> (push a)
                           (>> (push b)
                               (>> (push a)
                                   (push b)))))))))]
    [#x4c ; swap
     (>> (lsv>= 3 "swap")
         swap)]
    [#x4d ; select
     (>> (lsv>= 3 "swap")
         (>>= pop
              (λ (c)
                (>>= pop
                     (λ (b)
                       (>>= pop
                            (λ (a)
                              (>>= (is-zero c)
                                   (λ (zero?) (push (if zero? a b)))))))))))]
    [#x50 ; concat
     (>> (lsv>= 2 "concat")
         (stack-apply concat 2))]
    [#x52 ; substring3
     (>> (lsv>= 2 "substring3")
         (stack-apply substring3 3))]
    [#x60 ; balance
     (>> (lsv>= 2 "balance")
         (>> (in-mode 'Application "balance")
             (stack-apply balance 1)))]
    [#x62 ; app_local_get
     (>> (lsv>= 2 "app_local_get")
         (>> (in-mode 'Application "app_local_get")
             (stack-apply app-local-get 2)))]
    [#x64 ; app_global_get
     (>> (lsv>= 2 "app_global_get")
         (>> (in-mode 'Application "app_global_get")
             (stack-apply app-global-get 1)))]
    [#x65 ; app_global_get_ex
     (>> (lsv>= 2 "app_global_get")
         (>> (in-mode 'Application "app_global_get_ex")
             (>>= pop (λ (b) (>>= pop (λ (a) (app-global-get-ex a b)))))))]
    [#x66 ; app_local_put
     (>> (lsv>= 2 "app_local_put")
         (>> (in-mode 'Application "app_local_put")
             (>>= pop (λ (c) (>>= pop (λ (b) (>>= pop (λ (a) (app-local-put a b c)))))))))]
    [#x67 ; app_global_put
     (>> (lsv>= 2 "app_global_put")
         (>> (in-mode 'Application "app_global_put")
             (>>= pop (λ (b) (>>= pop (λ (a) (app-global-put a b)))))))]
    [#x68 ; app_local_del
     (>> (lsv>= 2 "app_local_del")
         (>> (in-mode 'Application "app_local_del")
             (>>= pop (λ (b) (>>= pop (λ (a) (app-local-del a b)))))))]
    [#x70 ; asset_holding_get
     (>> (lsv>= 2 "asset_holding_get")
         (>> (in-mode 'Application "asset_holding_get")
             (>>= (read-uint8 rb)
                  (λ (fi) (>>= pop (λ (b) (>>= pop (λ (a) (asset-holding-get a b fi)))))))))]
    [#x71 ; asset_params_get
     (>> (lsv>= 2 "asset_params_get")
         (>> (in-mode 'Application "asset_params_get")
             (>>= (read-uint8 rb)
                  (λ (fi) (>>= pop (λ (a) (asset-params-get a fi)))))))]
    [#x78 ; min_balance
     (>> (lsv>= 3 "min_balance")
         (>> (in-mode 'Application "min_balance")
             (stack-apply min-balance 1)))]
    [#x80 ; pushbytes
     (>> (lsv>= 3 "pushbytes")
         (>>= (read-bytes rb) push))]
    [#x81 ; pushint
     (>> (lsv>= 3 "pushint")
         (>>= (read-varuint rb) push))]
    [bc
     (display "0x")
     (displayln (number->string bc 16))
     (failure-cont)]))

; step : VM m s => m ()
(define (step vm)
  (match-define (VM (MonadPlus (Monad _ >>= >>) _ _) rb
                    _ ; panic
                    _ ; return!
                    _ ; logic-sig-version
                    _ ; in-mode
                    _ ; get-pc
                    _ ; set-pc
                    _ ; get-bytecode
                    _ ; get-intcblock
                    _ ; put-intcblock
                    _ ; get-bytecblock
                    _ ; put-bytecblock
                    _ ; push
                    _ ; pop
                    _ ; +
                    _ ; -
                    _ ; /
                    _ ; *
                    _ ; len
                    _ ; itob
                    _ ; btoi
                    _ ; %
                    _ ; mulw
                    _ ; addw
                    _ ; divmodw
                    _ ; is-zero
                    _ ; &&
                    _ ; !!
                    _ ; =
                    _ ; <
                    _ ; !
                    _ ; ~
                    _ ; concat
                    _ ; substring3
                    _ ; transaction
                    _ ; global
                    _ ; global-transaction
                    _ ; transaction-array
                    _ ; load
                    _ ; store
                    _ ; balance
                    _ ; min-balance
                    _ ; app-local-get
                    _ ; app-local-put
                    _ ; app-local-del
                    _ ; app-global-get
                    _ ; app-global-put
                    _ ; app-global-get-ex
                    _ ; asset-holding-get
                    _ ; asset-params-get
                    check-final
                    ) vm)
  (>> (>>= (read-opcode rb)
           (execute vm))
      check-final))

(define (logic-sig-version>= vm)
  (match-let ([(MonadPlus (Monad unit >>= _) _ _) (VM-MonadPlus vm)]
              [logic-sig-version (VM-logic-sig-version vm)]
              [panic (VM-panic vm)])
    (λ (target-lsv info)
      (>>= logic-sig-version
           (λ (lsv)
             (if (>= lsv target-lsv)
               (unit)
               (panic "LogicSig version = ~a but need >= ~a for ~a" lsv target-lsv info)))))))

(provide VM
         step
         logic-sig-version>=)
