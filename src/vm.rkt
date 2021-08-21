#lang racket/base
(require racket/match
         "monad.rkt"
         "read-byte.rkt")

(struct VM (MonadPlus ReadByte
                      fail!
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
                      itob
                      is-zero
                      &&
                      !!
                      ==
                      !
                      concat
                      transaction
                      global
                      global-transaction
                      transaction-array
                      load
                      store
                      balance
                      min-balance
                      app-local-get
                      asset-holding-get
                      check-final))

; execute : VM m s => byte → m ()
(define (execute vm)
  (match-define (VM (MonadPlus (Monad unit >>= >>) _ _) rb
                    fail!
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
                    itob
                    is-zero
                    &&
                    !!
                    ==
                    vm!
                    concat
                    transaction
                    global
                    global-transaction
                    transaction-array
                    load
                    store
                    balance
                    min-balance
                    app-local-get
                    asset-holding-get
                    _ ; check-final
                    ) vm)
  (define (logic-sig-version>= target-lsv info)
    (>>= logic-sig-version
         (λ (lsv)
           (if (>= lsv target-lsv)
             (unit)
             (fail! "LogicSig version = ~a but need >= ~a for ~a" lsv target-lsv info)))))
  (define (lookup-intcblock i)
    (>>= get-intcblock
         (λ (xs)
           (if (< i (length xs))
             (unit (list-ref xs i))
             (fail! "intcblock has ~a ints but index ~a requested" (length xs) i)))))
  (define (lookup-bytecblock i)
    (>>= get-bytecblock
         (λ (bss)
           (if (< i (length bss))
             (unit (list-ref bss i))
             (fail! "bytecblock has ~a bytes but index ~a requested" (length bss) i)))))
  (define continue (unit (list)))
  (define (jump offset)
    (>>= logic-sig-version
         (λ (lsv)
           (if (and (< offset 0)
                    (< lsv 4))
             (fail! "cannot jump backwards (offset ~a) in LogicSig version ~a < 4" offset lsv)
             (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
  (define (goto pc)
    (if (< pc 0)
      (fail! "cannot go to negative counter ~a" pc)
      (>>= get-bytecode
           (λ (bc)
             (if (> pc (bytes-length bc))
               (fail! "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
               (>>= logic-sig-version
                    (λ (lsv)
                      (if (and (= pc (bytes-length bc))
                               (< lsv 2))
                        (fail! "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                        (set-pc pc)))))))))
  (define (stack-apply f arity)
    (>>= (>>= (let loop ([n arity]
                         [xs (list)])
                (if (zero? n)
                  (apply unit xs)
                  (>>= pop (λ (x) (loop (sub1 n) (cons x xs))))))
              f)
         push))
  (match-lambda
    [#x00 ; err
     (fail! "err")]
    [#x08 ; +
     (stack-apply vm+ 2)]
    [#x09 ; -
     (stack-apply vm- 2)]
    [#x10 ; &&
     (stack-apply && 2)]
    [#x11 ; ||
     #;
     (>>= pop
          (λ (b)
            (>>= pop
                 (λ (a)
                   (>>= (>>= (is-zero a) 
                             (λ (a-zero?)
                               (if a-zero?
                                 (>>= (is-zero b)
                                      (λ (b-zero?)
                                        (if b-zero?
                                          ))))))
                        push)))))
     (stack-apply !! 2)]
    [#x12 ; ==
     (stack-apply == 2)]
    [#x13 ; !=
     (>> ((execute vm) #x12)   ; ==
         ((execute vm) #x14))] ; !
    [#x14 ; !
     (stack-apply vm! 1)]
    [#x16 ; itob
     (stack-apply itob 1)]
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
     (>> (logic-sig-version>= 2 "b")
         (>>= (read-int16 rb) jump))]
    [#x44 ; assert
     (>> (logic-sig-version>= 3 "assert")
         (>>= pop
              (λ (x)
                (>>= (is-zero x)
                     (λ (fail?)
                       (if fail?
                         (fail! "assert of ~a failed" x)
                         continue))))))]
    [#x49 ; dup
     (>>= pop
          (λ (x)
            (>> (push x)
                (push x))))]
    [#x50 ; concat
     (>> (logic-sig-version>= 2 "concat")
         (stack-apply concat 2))]
    [#x60 ; balance
     (>> (logic-sig-version>= 2 "balance")
         (>> (in-mode 'Application "balance")
             (stack-apply balance 1)))]
    [#x62 ; app_local_get
     (>> (logic-sig-version>= 2 "app_local_get")
         (>> (in-mode 'Application "app_local_get")
             (stack-apply app-local-get 2)))]
    [#x70 ; asset_holding_get
     (>> (logic-sig-version>= 2 "asset_holding_get")
         (>> (in-mode 'Application "asset_holding_get")
             (>>= (read-uint8 rb)
                  (λ (fi)
                    (>>= pop
                         (λ (b)
                           (>>= pop
                                (λ (a)
                                  (>>= (asset-holding-get a b fi)
                                       (match-lambda
                                         [#f
                                          (>> (push 0)
                                              (push 0))]
                                         [v
                                          (>> (push v)
                                              (push 1))]))))))))))]
    [#x78 ; min_balance
     (>> (logic-sig-version>= 3 "min_balance")
         (>> (in-mode 'Application "min_balance")
             (stack-apply min-balance 1)))]
    [#x80 ; pushbytes
     (>> (logic-sig-version>= 3 "pushbytes")
         (>>= (read-bytes rb) push))]
    [#x81 ; pushint
     (>> (logic-sig-version>= 3 "pushint")
         (>>= (read-varuint rb) push))]
    [bc
     (display "0x")
     (displayln (number->string bc 16))
     (failure-cont)]))

; step : VM m s => m ()
(define (step vm)
  (match-define (VM (MonadPlus (Monad _ >>= >>) _ _) rb
                    _ ; fail!
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
                    _ ; itob
                    _ ; is-zero
                    _ ; &&
                    _ ; !!
                    _ ; ==
                    _ ; !
                    _ ; concat
                    _ ; transaction
                    _ ; global
                    _ ; global-transaction
                    _ ; transaction-array
                    _ ; load
                    _ ; store
                    _ ; balance
                    _ ; min-balance
                    _ ; app-local-get
                    _ ; asset-holding-get
                    check-final
                    ) vm)
  (>> (>>= (read-opcode rb)
           (execute vm))
      check-final))

(provide VM
         step)
