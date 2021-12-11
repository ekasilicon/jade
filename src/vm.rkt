#lang racket/base
(require (only-in racket/list take)
         "static/object.rkt"
         "static/sumtype.rkt"
         (prefix-in i: "instruction.rkt"))

#;
(require (except-in racket/match ==)
         "static/record.rkt"
         "static/sumtype.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "logic-sig-version.rkt"
         "arithmetic-logic-unit.rkt"
         "internal-transaction.rkt"
         (prefix-in i: "instruction.rkt"))


#;
(record VM0 (monad+
             read-byte
             logic-sig-version
             in-mode
             get-bytecode
             get-pc set-pc
             push pop
             is-zero
             check-final
             panic))

#;
(define execute0
  (inc ()))


#|
(record VM1 VM0
        (sha256 keccak256 sha512-256 ed25519verify
         uint-alu
         !
         len
         itob btoi
         && \|\|
         mulw
         get-intcblock put-intcblock
         get-bytecblock put-bytecblock
         arg
         global
         transaction group-transaction
         load store))

(define execute1
  (mix (goto
        continue
        lookup-intcblock lookup-bytecblock
        unit >>= >> mplus mzero)
       ) (execute1 vm)
  (match-let* (
               [(Monad+ unit >>= mplus) monad+])
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
    (define continue (unit))
    (define (goto pc)
      (if (< pc 0)
        (panic "cannot go to negative counter ~a" pc)
        (>>= get-bytecode
             (λ (bc)
               (if (> pc (bytes-length bc))
                 (panic "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
                 (>>= (logic-sig-version lsv) 
                      (λ (lsv)
                        (if (and (= pc (bytes-length bc))
                                 (< lsv 2))
                          (panic "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                          (set-pc pc)))))))))
    (define (jump offset)
      (>>= (logic-sig-version lsv) 
           (λ (lsv)
             (if (and (< offset 0)
                      (< lsv 4))
               (panic "cannot jump backwards (offset ~a) in LogicSigVersion ~a < 4" offset lsv)
               (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
    (define (pop [n 1])
      (let loop ([n n]
                 [xs (list)])
        (if (zero? n)
          (apply unit xs)
          (>>= vm:pop (λ (x) (loop (sub1 n) (cons x xs)))))))
    (define (push . xs) (foldr (λ (x m) (>> (vm:push x) m)) (unit) xs))
    (define swap (>>= (pop 2) (λ (a b) (push b a))))
    (define (primitive-apply f stack-arity . xs)
      (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))
    (sumtype-case-lambda i:Instruction1
      [(i:err)
       (panic "err")]
      [(i:sha256)
       (primitive-apply sha256 1)]
      [(i:keccak256)
       (primitive-apply keccak256 1)]
      [(i:sha512_256)
       (primitive-apply sha512-256 1)]
      [(i:ed25519verify)
       (primitive-apply ed25519verify 3)]
      [(i:+)
       (primitive-apply u+ 2)]
      [(i:-)
       (primitive-apply u- 2)]
      [(i:/)
       (>>= (pop 2)
            (λ (a b)
              (>>= (is-zero b)
                   (λ (zero?)
                     (if zero?
                       (panic "/: ~a is 0" b)
                       (>>= (u/ a b) push))))))]
      [(i:*)
       (primitive-apply u* 2)]
      [(i:<)
       (primitive-apply u< 2)]
      [(i:>)
       (>> swap
           (primitive-apply u< 2))]
      [(i:<=)
       (>> swap
           (primitive-apply u< 2)
           (primitive-apply ! 1))]
      [(i:>=)
       (>> (primitive-apply u< 2)
           (primitive-apply ! 1))]
      [(i:&&)
       (primitive-apply && 2)]
      [(i:\|\|)
       (primitive-apply \|\| 2)]
      [(i:==)
       (primitive-apply u== 2)]
      [(i:!=)
       (>> (primitive-apply u== 2)
           (primitive-apply ! 1))]
      [(i:!)
       (primitive-apply ! 1)]
      [(i:len)
       (primitive-apply len 1)]
      [(i:itob)
       (primitive-apply itob 1)]
      [(i:btoi)
       (primitive-apply btoi 1)]
      [(i:%)
       (>>= (pop 2)
            (λ (a b)
              (>>= (is-zero b)
                   (λ (zero?)
                     (if zero?
                       (panic "%: ~a is 0" b)
                       (>>= (u% a b) push))))))]
      [(i:\|)
       (primitive-apply u\| 2)]
      [(i:&)
       (primitive-apply u& 2)]
      [(i:^)
       (primitive-apply u^ 2)]
      [(i:~)
       (primitive-apply u~ 1)]
      [(i:mulw)
       (primitive-apply mulw 2)]
      [(i:intcblock [uints ns])
       (put-intcblock ns)]
      [(i:intc i)
       (>>= (lookup-intcblock i) push)]
      [(i:intc_0)
       (>>= (lookup-intcblock 0) push)]
      [(i:intc_1)
       (>>= (lookup-intcblock 1) push)]
      [(i:intc_2)
       (>>= (lookup-intcblock 2) push)]
      [(i:intc_3)
       (>>= (lookup-intcblock 3) push)]
      [(i:bytecblock [bytess bss])
       (put-bytecblock bss)]
      [(i:bytec i)
       (>>= (lookup-bytecblock i) push)]
      [(i:bytec_0)
       (>>= (lookup-bytecblock 0) push)]
      [(i:bytec_1)
       (>>= (lookup-bytecblock 1) push)]
      [(i:bytec_2)
       (>>= (lookup-bytecblock 2) push)]
      [(i:bytec_3)
       (>>= (lookup-bytecblock 3) push)]
      [(i:arg n)
       (>> (in-mode 'Signature "arg")
           (primitive-apply arg 0 n))]
      [(i:arg_0)
       (>> (in-mode 'Signature "arg_0")
           (primitive-apply arg 0 0))]
      [(i:arg_1)
       (>> (in-mode 'Signature "arg_1")
           (primitive-apply arg 0 1))]
      [(i:arg_2)
       (>> (in-mode 'Signature "arg_2")
           (primitive-apply arg 0 2))]
      [(i:arg_3)
       (>> (in-mode 'Signature "arg_3")
           (primitive-apply arg 0 3))]
      [(i:txn [field f])
       (primitive-apply transaction 0 f)]
      [(i:global [field f])
       (primitive-apply global 0 f)]
      [(i:gtxn [group-index gi] [field f])
       (primitive-apply group-transaction 0 gi f)]
      [(i:load i)
       (primitive-apply load 0 i)]
      [(i:store i)
       (primitive-apply store 1 i)]
      [(i:bnz offset)
       (>>= (>>= (pop) is-zero)
            (λ (stay?)
              (if stay?
                continue
                (jump offset))))]
      [(i:pop)
       (>> (pop)
           continue)]
      [(i:dup)
       (>>= (pop) (λ (x) (push x x)))])))


(define (execute1 vm)
  (match-let* ([(VM1 monad+
                     [read-byte rb]
                     [logic-sig-version lsv]
                     in-mode
                     get-bytecode
                     get-pc set-pc
                     [push vm:push] [pop vm:pop]
                     is-zero
                     panic

                     sha256 keccak256 sha512-256 ed25519verify
                     [uint-alu (ArithmeticLogicUnit
                                [+  u+]
                                [-  u-]
                                [/  u/]
                                [*  u*]
                                [%  u%]
                                [&  u&]
                                [\| u\|]
                                [^  u^]
                                [~  u~]
                                [<  u<]
                                [== u==])]
                     !
                     len
                     itob btoi
                     && \|\|
                     mulw
                     get-intcblock put-intcblock
                     get-bytecblock put-bytecblock
                     arg
                     global
                     transaction group-transaction
                     load store)
                vm]
               [(Monad+ unit >>= mplus) monad+])
    (define >> (derive->> monad+))
    (define mzero (mplus))
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
    (define continue (unit))
    (define (goto pc)
      (if (< pc 0)
        (panic "cannot go to negative counter ~a" pc)
        (>>= get-bytecode
             (λ (bc)
               (if (> pc (bytes-length bc))
                 (panic "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
                 (>>= (logic-sig-version lsv) 
                      (λ (lsv)
                        (if (and (= pc (bytes-length bc))
                                 (< lsv 2))
                          (panic "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                          (set-pc pc)))))))))
    (define (jump offset)
      (>>= (logic-sig-version lsv) 
           (λ (lsv)
             (if (and (< offset 0)
                      (< lsv 4))
               (panic "cannot jump backwards (offset ~a) in LogicSigVersion ~a < 4" offset lsv)
               (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
    (define (pop [n 1])
      (let loop ([n n]
                 [xs (list)])
        (if (zero? n)
          (apply unit xs)
          (>>= vm:pop (λ (x) (loop (sub1 n) (cons x xs)))))))
    (define (push . xs) (foldr (λ (x m) (>> (vm:push x) m)) (unit) xs))
    (define swap (>>= (pop 2) (λ (a b) (push b a))))
    (define (primitive-apply f stack-arity . xs)
      (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))
    (sumtype-case-lambda i:Instruction1
      [(i:err)
       (panic "err")]
      [(i:sha256)
       (primitive-apply sha256 1)]
      [(i:keccak256)
       (primitive-apply keccak256 1)]
      [(i:sha512_256)
       (primitive-apply sha512-256 1)]
      [(i:ed25519verify)
       (primitive-apply ed25519verify 3)]
      [(i:+)
       (primitive-apply u+ 2)]
      [(i:-)
       (primitive-apply u- 2)]
      [(i:/)
       (>>= (pop 2)
            (λ (a b)
              (>>= (is-zero b)
                   (λ (zero?)
                     (if zero?
                       (panic "/: ~a is 0" b)
                       (>>= (u/ a b) push))))))]
      [(i:*)
       (primitive-apply u* 2)]
      [(i:<)
       (primitive-apply u< 2)]
      [(i:>)
       (>> swap
           (primitive-apply u< 2))]
      [(i:<=)
       (>> swap
           (primitive-apply u< 2)
           (primitive-apply ! 1))]
      [(i:>=)
       (>> (primitive-apply u< 2)
           (primitive-apply ! 1))]
      [(i:&&)
       (primitive-apply && 2)]
      [(i:\|\|)
       (primitive-apply \|\| 2)]
      [(i:==)
       (primitive-apply u== 2)]
      [(i:!=)
       (>> (primitive-apply u== 2)
           (primitive-apply ! 1))]
      [(i:!)
       (primitive-apply ! 1)]
      [(i:len)
       (primitive-apply len 1)]
      [(i:itob)
       (primitive-apply itob 1)]
      [(i:btoi)
       (primitive-apply btoi 1)]
      [(i:%)
       (>>= (pop 2)
            (λ (a b)
              (>>= (is-zero b)
                   (λ (zero?)
                     (if zero?
                       (panic "%: ~a is 0" b)
                       (>>= (u% a b) push))))))]
      [(i:\|)
       (primitive-apply u\| 2)]
      [(i:&)
       (primitive-apply u& 2)]
      [(i:^)
       (primitive-apply u^ 2)]
      [(i:~)
       (primitive-apply u~ 1)]
      [(i:mulw)
       (primitive-apply mulw 2)]
      [(i:intcblock [uints ns])
       (put-intcblock ns)]
      [(i:intc i)
       (>>= (lookup-intcblock i) push)]
      [(i:intc_0)
       (>>= (lookup-intcblock 0) push)]
      [(i:intc_1)
       (>>= (lookup-intcblock 1) push)]
      [(i:intc_2)
       (>>= (lookup-intcblock 2) push)]
      [(i:intc_3)
       (>>= (lookup-intcblock 3) push)]
      [(i:bytecblock [bytess bss])
       (put-bytecblock bss)]
      [(i:bytec i)
       (>>= (lookup-bytecblock i) push)]
      [(i:bytec_0)
       (>>= (lookup-bytecblock 0) push)]
      [(i:bytec_1)
       (>>= (lookup-bytecblock 1) push)]
      [(i:bytec_2)
       (>>= (lookup-bytecblock 2) push)]
      [(i:bytec_3)
       (>>= (lookup-bytecblock 3) push)]
      [(i:arg n)
       (>> (in-mode 'Signature "arg")
           (primitive-apply arg 0 n))]
      [(i:arg_0)
       (>> (in-mode 'Signature "arg_0")
           (primitive-apply arg 0 0))]
      [(i:arg_1)
       (>> (in-mode 'Signature "arg_1")
           (primitive-apply arg 0 1))]
      [(i:arg_2)
       (>> (in-mode 'Signature "arg_2")
           (primitive-apply arg 0 2))]
      [(i:arg_3)
       (>> (in-mode 'Signature "arg_3")
           (primitive-apply arg 0 3))]
      [(i:txn [field f])
       (primitive-apply transaction 0 f)]
      [(i:global [field f])
       (primitive-apply global 0 f)]
      [(i:gtxn [group-index gi] [field f])
       (primitive-apply group-transaction 0 gi f)]
      [(i:load i)
       (primitive-apply load 0 i)]
      [(i:store i)
       (primitive-apply store 1 i)]
      [(i:bnz offset)
       (>>= (>>= (pop) is-zero)
            (λ (stay?)
              (if stay?
                continue
                (jump offset))))]
      [(i:pop)
       (>> (pop)
           continue)]
      [(i:dup)
       (>>= (pop) (λ (x) (push x x)))])))

(record VM2 VM1
        (addw
         transaction-array group-transaction-array
         return
         concat
         substring
         balance
         app-opted-in
         app-local-get app-local-put app-local-del app-local-get-ex
         app-global-get app-global-put app-global-del app-global-get-ex
         asset-holding-get asset-params-get))

(define (execute2 vm)
  (match-let* ([(VM2 monad+
                     [read-byte rb]
                           [logic-sig-version lsv]
                           in-mode
                           get-bytecode
                           get-pc set-pc
                           [push vm:push] [pop vm:pop]
                           is-zero
                           panic
                           addw
                           transaction-array group-transaction-array
                           return
                           concat
                           substring
                           balance
                           app-opted-in
                           app-local-get app-local-put app-local-del app-local-get-ex
                           app-global-get app-global-put app-global-del app-global-get-ex
                           asset-holding-get asset-params-get)
                vm]
               [(Monad+ unit >>= mplus) monad+])
    (define >> (derive->> monad+))
    (define continue (unit))
    (define (goto pc)
      (if (< pc 0)
        (panic "cannot go to negative counter ~a" pc)
        (>>= get-bytecode
             (λ (bc)
               (if (> pc (bytes-length bc))
                 (panic "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
                 (>>= (logic-sig-version lsv) 
                      (λ (lsv)
                        (if (and (= pc (bytes-length bc))
                                 (< lsv 2))
                          (panic "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                          (set-pc pc)))))))))
    (define (jump offset)
      (>>= (logic-sig-version lsv) 
           (λ (lsv)
             (if (and (< offset 0)
                      (< lsv 4))
               (panic "cannot jump backwards (offset ~a) in LogicSigVersion ~a < 4" offset lsv)
               (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
    (define (pop [n 1])
      (let loop ([n n]
                 [xs (list)])
        (if (zero? n)
          (apply unit xs)
          (>>= vm:pop (λ (x) (loop (sub1 n) (cons x xs)))))))
    (define (push . xs) (foldr (λ (x m) (>> (vm:push x) m)) (unit) xs))
    (define (primitive-apply f stack-arity . xs)
      (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))
    (sumtype-case-lambda i:Instruction2
      [(i:Instruction1 instr)
       ((execute1 vm) instr)]
      [(i:addw)
       (primitive-apply addw 2)]
      [(i:txna [field f] [array-index ai])
       (primitive-apply transaction-array 0 f ai)]
      [(i:gtxna [group-index gi] [field f] [array-index ai])
       (primitive-apply group-transaction-array 0 gi f ai)]
      [(i:bz offset)
       (>>= (>>= (pop) is-zero)
            (λ (jump?)
              (if jump?
                (jump offset)
                continue)))]
      [(i:b offset)
       (jump offset)]
      [(i:return)
       (>>= (pop) return)]
      [(i:dup2)
       (>>= (pop 2) (λ (a b) (push a b a b)))]
      [(i:concat)
       (primitive-apply concat 2)]
      [(i:substring [start s] [end e])
       (>>= (>>= (pop) (λ (a) (substring a s e))) push)]
      [(i:substring3)
       (primitive-apply substring 3)]
      [(i:balance)
       (>> (in-mode 'Application "balance")
           (primitive-apply balance 1))]
      [(i:app_opted_in)
       (>> (in-mode 'Application "app_opted_in")
           (primitive-apply app-opted-in 2))]
      [(i:app_local_get)
       (>> (in-mode 'Application "app_local_get")
           (primitive-apply app-local-get 2))]
      [(i:app_local_get_ex)
       (>> (in-mode 'Application "app_local_get_ex")
           (primitive-apply app-local-get-ex 3))]
      [(i:app_global_get)
       (>> (in-mode 'Application "app_global_get")
           (primitive-apply app-global-get 1))]
      [(i:app_global_get_ex)
       (>> (in-mode 'Application "app_global_get_ex")
           (primitive-apply app-global-get-ex 2))]
      [(i:app_local_put)
       (>> (in-mode 'Application "app_local_put")
           (primitive-apply app-local-put 3))]
      [(i:app_global_put)
       (>> (in-mode 'Application "app_global_put")
           (primitive-apply app-global-put 2))]
      [(i:app_local_del)
       (>> (in-mode 'Application "app_local_del")
           (primitive-apply app-local-del 2))]
      [(i:app_global_del)
       (>> (in-mode 'Application "app_global_del")
           (primitive-apply app-global-del 1))]
      [(i:asset_holding_get [field f])
       (>> (in-mode 'Application "asset_holding_get")
           (>>= (pop 2) (λ (a b) (asset-holding-get a b f))))]
      [(i:asset_params_get [field f])
       (>> (in-mode 'Application "asset_params_get")
           (>>= (>>= (pop) (λ (a) (asset-params-get a f))) push))])))


(record VM3 VM2
        (getbit setbit
         getbyte setbyte
         min-balance))

(define (execute3 vm)
  (match-let* ([(VM3 monad+
                     [read-byte rb]
                     [logic-sig-version lsv]
                     in-mode
                     get-bytecode
                     get-pc set-pc
                     [push vm:push] [pop vm:pop]
                     is-zero
                     panic

                     group-transaction group-transaction-array
                     
                     getbit setbit
                     getbyte setbyte
                     min-balance)
                vm]
               [(Monad+ unit >>= mplus) monad+])
    (define >> (derive->> monad+))
    (define continue (unit))
    (define (pop [n 1])
      (let loop ([n n]
                 [xs (list)])
        (if (zero? n)
          (apply unit xs)
          (>>= vm:pop (λ (x) (loop (sub1 n) (cons x xs)))))))
    (define (push . xs) (foldr (λ (x m) (>> (vm:push x) m)) (unit) xs))
    (define swap (>>= (pop 2) (λ (a b) (push b a))))
    (define (primitive-apply f stack-arity . xs)
      (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))
    (sumtype-case-lambda i:Instruction3
      [(i:Instruction2 instr)
       ((execute2 vm) instr)]
      [(i:gtxns [field f])
       (>>= (>>= (pop) (λ (gi) (group-transaction gi f))) push)]
      [(i:gtxnsa [field f] [array-index ai])
       (>>= (>>= (pop) (λ (gi) (group-transaction-array gi f ai))) push)]
      [(i:assert)
       (>>= (pop)
            (λ (x)
              (>>= (is-zero x)
                   (λ (fail?)
                     (if fail?
                       (panic "assert: ~v" x)
                       continue)))))]
      [(i:dig n)
       (>>= (let loop ([n n])
              (>>= (pop)
                   (λ (x)
                     (if (zero? n)
                       (>> (push x)
                           (unit x))
                       (>>= (loop (sub1 n))
                            (λ (y)
                              (push x)
                              (unit y)))))))
            push)]
      [(i:swap)
       swap]
      [(i:select)
       (>>= (pop 3)
            (λ (a b c)
              (>>= (is-zero c)
                   (λ (zero?) (push (if zero? a b))))))]
      [(i:getbit)
       (primitive-apply getbit 2)]
      [(i:setbit)
       (primitive-apply setbit 3)]
      [(i:getbyte)
       (primitive-apply getbyte 2)]
      [(i:setbyte)
       (primitive-apply setbyte 3)]
      [(i:min_balance)
       (>> (in-mode 'Application "min_balance")
           (primitive-apply min-balance 1))]
      [(i:pushbytes [bytes bs])
       (push bs)]
      [(i:pushint [uint n])
       (push n)])))


#|
(record VM4 (base
             exp
             divmodw
             expw
             group-load
             group-aid
             push-call
             pop-call
             shl
             shr
             bitlen
             bytes-alu
             bzero))

(sumtype-case-lambda i:Instruction
  [(i:divmodw)
   (primitive-apply divmodw 4)]
  [(i:gload [group-index gi] i)
   (>> (in-mode 'Application "gload")
       (primitive-apply group-load 0 gi i))]
  [(i:gloads i)
   (>> (in-mode 'Application "gloads")
       (>>= (>>= (pop) (λ (gi) (group-load gi i))) push))]
  [(i:gaid [group-index gi])
   (>> (in-mode 'Application "gaid")
       (primitive-apply group-aid 0 gi))]
  [(i:gaids)
   (>> (in-mode 'Application "gaids")
       (primitive-apply group-aid 1))]
  [(i:callsub offset)
   (>>= get-pc
        (λ (ret-pc)
          (>> (push-call ret-pc)
              (jump offset))))]
  [(i:retsub)
   (>>= pop-call goto)]
  [(i:shl)
   (primitive-apply shl 2)]
  [(i:shr)
   (primitive-apply shr 2)]
  [(i:sqrt)
   (primitive-apply sqrt 1)]
  [(i:bitlen)
   (primitive-apply bitlen 1)]
  [(i:exp)
   (>>= (pop 2)
        (λ (a b)
          (>>= (is-zero a)
               (λ (is-zero-a?)
                 (>>= (is-zero b)
                      (λ (is-zero-b?)
                        (if (and is-zero-a? is-zero-b?)
                          (panic "exp: both ~a and ~a are zero" a b)
                          (>>= (exp a b) push))))))))]
  [(i:expw)
   (>>= (pop 2)
        (λ (a b)
          (>>= (is-zero a)
               (λ (is-zero-a?)
                 (>>= (is-zero b)
                      (λ (is-zero-b?)
                        (if (and is-zero-a? is-zero-b?)
                          (panic "expw: both ~a and ~a are zero" a b)
                          (>>= (expw a b) push))))))))]
  [(i:b+)
   (primitive-apply b+ 2)]
  [(i:b-)
   (primitive-apply b- 2)]
  [(i:b/)
   (>>= (pop 2)
        (λ (a b)
          (>>= (is-zero b)
               (λ (is-zero?)
                 (if is-zero?
                   (panic "b/: ~a is 0" b)
                   (>>= (b/ a b) push))))))]
  [(i:b*)
   (primitive-apply b* 2)]
  [(i:b<)
   (primitive-apply b< 2)]
  [(i:b>)
   (>> swap
       (primitive-apply b< 2))]
  [(i:b<=)
   (>> swap
       (primitive-apply b< 2)
       (primitive-apply ! 1))]
  [(i:b>=)
   (>> (primitive-apply b< 2)
       (primitive-apply ! 1))]
  [(i:b==)
   (primitive-apply b== 2)]
  [(i:b!=)
   (>> (primitive-apply b== 2)
       (primitive-apply ! 1))]
  [(i:b%)
   (>>= (pop 2)
        (λ (a b)
          (>>= (is-zero b)
               (λ (is-zero?)
                 (if is-zero?
                   (panic "b%: ~a is 0" b)
                   (>>= (b% a b) push))))))]
  [(i:b\|)
   (primitive-apply b\| 2)]
  [(i:b&)
   (primitive-apply b& 2)]
  [(i:b^)
   (primitive-apply b^ 2)]
  [(i:b~)
   (primitive-apply b~ 1)]
  [(i:bzero)
   (primitive-apply bzero 1)])

(record VM5 (base
             ecdsa-verify
             ecdsa-pk-decompress
             ecdsa-pk-recover
             extract
             extract-uint
             app-params-get
             log
             internal-transaction ; InternalTransaction
             args))

(sumtype-case-lambda i:Instruction
  [(i:ecdsa_verify v)
   (primitive-apply ecdsa-verify 5 v)]
  [(i:ecdsa_pk_decompress v)
   (primitive-apply ecdsa-pk-decompress 1 v)]
  [(i:ecdsa_pk_recover v)
   (primitive-apply ecdsa-pk-recover 4 v)]
  [(i:loads)
   (primitive-apply load 1)]
  [(i:stores)
   (primitive-apply store 2)]
  [(i:cover n)
   (>>= (pop)
        (λ (x)
          (let loop ([n n])
            (if (zero? n)
              (push x)
              (>>= (pop)
                   (λ (x)
                     (>> (loop (sub1 n))
                         (push x))))))))]
  [(i:uncover n)
   (>>= (let loop ([n n])
          (if (zero? n)
            (pop)
            (>>= (pop)
                 (λ (x)
                   (>>= (loop (sub1 n))
                        (λ (y)
                          (>> (push x)
                              (unit y))))))))
        push)]
  [(i:extract [start s] [length ℓ])
   (>>= (>>= (pop) (λ (a) (extract a s ℓ))) push)]
  [(i:extract3)
   (primitive-apply extract 3)]
  [(i:extract_uint16)
   (primitive-apply extract-uint 2 2)]
  [(i:extract_uint32)
   (primitive-apply extract-uint 2 4)]
  [(i:extract_uint64)
   (primitive-apply extract-uint 2 8)]
  [(i:app_params_get [field f])
   (>> (in-mode 'Application "app_params_get")
       (>>= (>>= (pop) (λ (a) (app-params-get a f))) push))]
  [(i:log)
   (>> (in-mode 'Application "log")
       (primitive-apply log 1))]
  [(i:itxn_begin)
   (>> (in-mode 'Application "itxn_begin")
       itxn-begin)]
  [(i:itxn_field [field f])
   (>> (in-mode 'Application "itxn_field")
       (primitive-apply itxn-field 1 f))]
  [(i:itxn_submit)
   (>> (in-mode 'Application "itxn_submit")
       itxn-submit)]
  [(i:itxn [field f])
   (>> (in-mode 'Application "itxn")
       (primitive-apply itxn-access 0 f))]
  [(i:itxna [field f] [array-index ai])
   (>> (in-mode 'Application "itxna")
       (primitive-apply itxn-array 0 f ai))]
  [(i:txnas [field f])
   (primitive-apply transaction-array 1 f)]
  [(i:gtxnas [group-index gi] [field f])
   (primitive-apply group-transaction-array 1 gi f)]
  [(i:gtxnsas [field f])
   (>>= (>>= (pop 2) (λ (gi ai) (group-transaction-array gi f ai))) push)]
  [(i:args)
   (>> (in-mode 'Signature "args")
       (primitive-apply args 1))])

(record VM6 (base
             internal-transaction)) ; InternalTransaction6 extending InternalTransaction

(define (execute vm))
(sumtype-case-lambda i:Instruction6
  [(i:Instruction5 instr)
   ]
  [(i:itxn_next)
   (>> (in-mode 'Application "itxn_next")
       itxn-next)])

; execute : vm m s => Instruction → m ()
(define (execute vm)
  (match-let* ([(VM [monad+ (Monad+ [monad (and monad (Monad unit >>=))])]
                    [read-byte rb]
                    [logic-sig-version lsv]
                    panic
                    return
                    in-mode
                    get-pc
                    set-pc
                    get-bytecode
                    get-intcblock
                    put-intcblock
                    get-bytecblock
                    put-bytecblock
                    arg
                    [push vm:push]
                    [pop vm:pop]
                    push-call
                    pop-call
                    sha256
                    keccak256
                    sha512-256
                    ed25519verify
                    ecdsa-verify
                    ecdsa-pk-decompress
                    ecdsa-pk-recover
                    [uint-alu (ArithmeticLogicUnit
                               [+  u+]
                               [-  u-]
                               [/  u/]
                               [*  u*]
                               [%  u%]
                               [&  u&]
                               [\| u\|]
                               [^  u^]
                               [~  u~]
                               [<  u<]
                               [== u==])]
                    !
                    len
                    itob
                    btoi
                    mulw
                    addw
                    divmodw
                    is-zero
                    &&
                    \|\|
                    concat
                    substring
                    getbyte
                    transaction-array
                    group-transaction-array
                    group-aid
                    group-load
                    load
                    store
                    getbit
                    setbit
                    ;getbyte
                    setbyte
                    extract
                    extract-uint
                    balance
                    min-balance
                    app-opted-in
                    app-local-get
                    app-local-put
                    app-local-del
                    app-local-get-ex
                    app-global-get
                    app-global-put
                    app-global-del
                    app-global-get-ex
                    asset-holding-get
                    asset-params-get
                    app-params-get
                    shl
                    shr
                    bitlen
                    expw
                    [bytes-alu (ArithmeticLogicUnit
                                [+  b+]
                                [-  b-]
                                [/  b/]
                                [*  b*]
                                [%  b%]
                                [&  b&]
                                [\| b\|]
                                [^  b^]
                                [~  b~]
                                [<  b<]
                                [== b==])] 
                    bzero
                    [internal-transaction
                     (InternalTransaction [begin  itxn-begin]
                                          [next   itxn-next]
                                          [field  itxn-field]
                                          [submit itxn-submit]
                                          [access itxn-access]
                                          [array  itxn-array])]
                    args)
                vm]
               [>> (>> monad)])
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
      (>>= (logic-sig-version lsv) 
           (λ (lsv)
             (if (and (< offset 0)
                      (< lsv 4))
               (panic "cannot jump backwards (offset ~a) in LogicSigVersion ~a < 4" offset lsv)
               (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
    (define (goto pc)
      (if (< pc 0)
        (panic "cannot go to negative counter ~a" pc)
        (>>= get-bytecode
             (λ (bc)
               (if (> pc (bytes-length bc))
                 (panic "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
                 (>>= (logic-sig-version lsv) 
                      (λ (lsv)
                        (if (and (= pc (bytes-length bc))
                                 (< lsv 2))
                          (panic "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                          (set-pc pc)))))))))
    (define (pop [n 1])
      (let loop ([n n]
                 [xs (list)])
        (if (zero? n)
          (apply unit xs)
          (>>= vm:pop (λ (x) (loop (sub1 n) (cons x xs)))))))
    (define (push . xs) (foldr (λ (x m) (>> (vm:push x) m)) (unit) xs))
    (define swap (>>= (pop 2) (λ (a b) (push b a))))
    (define (primitive-apply f stack-arity . xs)
      (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))
    (define-syntax-rule (machine-fail instr)
      (error 'vm "unimplemented instruction ~a" 'instr))
    (λ (instr)
      (sumtype-case i:Instruction instr
        [(i:err)
         (panic "err")]
        [(i:sha256)
         (primitive-apply sha256 1)]
        [(i:keccak256)
         (primitive-apply keccak256 1)]
        [(i:sha512_256)
         (primitive-apply sha512-256 1)]
        [(i:ed25519verify)
         (primitive-apply ed25519verify 3)]
        [(i:ecdsa_verify v)
         (primitive-apply ecdsa-verify 5 v)]
        [(i:ecdsa_pk_decompress v)
         (primitive-apply ecdsa-pk-decompress 1 v)]
        [(i:ecdsa_pk_recover v)
         (primitive-apply ecdsa-pk-recover 4 v)]
        [(i:+)
         (primitive-apply u+ 2)]
        [(i:-)
         (primitive-apply u- 2)]
        [(i:/)
         (>>= (pop 2)
              (λ (a b)
                (>>= (is-zero b)
                     (λ (zero?)
                       (if zero?
                         (panic "/: ~a is 0" b)
                         (>>= (u/ a b) push))))))]
        [(i:*)
         (primitive-apply u* 2)]
        [(i:<)
         (primitive-apply u< 2)]
        [(i:>)
         (>> swap
             (primitive-apply u< 2))]
        [(i:<=)
         (>> swap
             (primitive-apply u< 2)
             (primitive-apply ! 1))]
        [(i:>=)
         (>> (primitive-apply u< 2)
             (primitive-apply ! 1))]
        [(i:&&)
         (primitive-apply && 2)]
        [(i:\|\|)
         (primitive-apply \|\| 2)]
        [(i:==)
         (primitive-apply u== 2)]
        [(i:!=)
         (>> (primitive-apply u== 2)
             (primitive-apply ! 1))]
        [(i:!)
         (primitive-apply ! 1)]
        [(i:len)
         (primitive-apply len 1)]
        [(i:itob)
         (primitive-apply itob 1)]
        [(i:btoi)
         (primitive-apply btoi 1)]
        [(i:%)
         (>>= (pop 2)
              (λ (a b)
                (>>= (is-zero b)
                     (λ (zero?)
                       (if zero?
                         (panic "%: ~a is 0" b)
                         (>>= (u% a b) push))))))]
        [(i:\|)
         (primitive-apply u\| 2)]
        [(i:&)
         (primitive-apply u& 2)]
        [(i:^)
         (primitive-apply u^ 2)]
        [(i:~)
         (primitive-apply u~ 1)]
        [(i:mulw)
         (primitive-apply mulw 2)]
        [(i:addw)
         (primitive-apply addw 2)]
        [(i:divmodw)
         (primitive-apply divmodw 4)]
        [(i:intcblock [uints ns])
         (put-intcblock ns)]
        [(i:intc i)
         (>>= (lookup-intcblock i) push)]
        [(i:intc_0)
         (>>= (lookup-intcblock 0) push)]
        [(i:intc_1)
         (>>= (lookup-intcblock 1) push)]
        [(i:intc_2)
         (>>= (lookup-intcblock 2) push)]
        [(i:intc_3)
         (>>= (lookup-intcblock 3) push)]
        [(i:bytecblock [bytess bss])
         (put-bytecblock bss)]
        [(i:bytec i)
         (>>= (lookup-bytecblock i) push)]
        [(i:bytec_0)
         (>>= (lookup-bytecblock 0) push)]
        [(i:bytec_1)
         (>>= (lookup-bytecblock 1) push)]
        [(i:bytec_2)
         (>>= (lookup-bytecblock 2) push)]
        [(i:bytec_3)
         (>>= (lookup-bytecblock 3) push)]
        [(i:arg n)
         (>> (in-mode 'Signature "arg")
             (primitive-apply arg 0 n))]
        [(i:arg_0)
         (>> (in-mode 'Signature "arg_0")
             (primitive-apply arg 0 0))]
        [(i:arg_1)
         (>> (in-mode 'Signature "arg_1")
             (primitive-apply arg 0 1))]
        [(i:arg_2)
         (>> (in-mode 'Signature "arg_2")
             (primitive-apply arg 0 2))]
        [(i:arg_3)
         (>> (in-mode 'Signature "arg_3")
             (primitive-apply arg 0 3))]
        [(i:txn [field f])
         (primitive-apply transaction 0 f)]
        [(i:global [field f])
         (primitive-apply global 0 f)]
        [(i:gtxn [group-index gi] [field f])
         (primitive-apply group-transaction 0 gi f)]
        [(i:load i)
         (primitive-apply load 0 i)]
        [(i:store i)
         (primitive-apply store 1 i)]
        [(i:txna [field f] [array-index ai])
         (primitive-apply transaction-array 0 f ai)]
        [(i:gtxna [group-index gi] [field f] [array-index ai])
         (primitive-apply group-transaction-array 0 gi f ai)]
        [(i:gtxns [field f])
         (>>= (>>= (pop) (λ (gi) (group-transaction gi f))) push)]
        [(i:gtxnsa [field f] [array-index ai])
         (>>= (>>= (pop) (λ (gi) (group-transaction-array gi f ai))) push)]
        [(i:gload [group-index gi] i)
         (>> (in-mode 'Application "gload")
             (primitive-apply group-load 0 gi i))]
        [(i:gloads i)
         (>> (in-mode 'Application "gloads")
             (>>= (>>= (pop) (λ (gi) (group-load gi i))) push))]
        [(i:gaid [group-index gi])
         (>> (in-mode 'Application "gaid")
             (primitive-apply group-aid 0 gi))]
        [(i:gaids)
         (>> (in-mode 'Application "gaids")
             (primitive-apply group-aid 1))]
        [(i:loads)
         (primitive-apply load 1)]
        [(i:stores)
         (primitive-apply store 2)]
        [(i:bnz offset)
         (>>= (>>= (pop) is-zero)
              (λ (stay?)
                (if stay?
                  continue
                  (jump offset))))]
        [(i:bz offset)
         (>>= (>>= (pop) is-zero)
              (λ (jump?)
                (if jump?
                  (jump offset)
                  continue)))]
        [(i:b offset)
         (jump offset)]
        [(i:return)
         (>>= (pop) return)]
        [(i:assert)
         (>>= (pop)
              (λ (x)
                (>>= (is-zero x)
                     (λ (fail?)
                       (if fail?
                         (panic "assert: ~v" x)
                         continue)))))]
        [(i:pop)
         (>> (pop)
             continue)]
        [(i:dup)
         (>>= (pop) (λ (x) (push x x)))]
        [(i:dup2)
         (>>= (pop 2) (λ (a b) (push a b a b)))]
        [(i:dig n)
         (>>= (let loop ([n n])
                (>>= (pop)
                     (λ (x)
                       (if (zero? n)
                         (>> (push x)
                             (unit x))
                         (>>= (loop (sub1 n))
                              (λ (y)
                                (push x)
                                (unit y)))))))
              push)]
        [(i:swap)
         swap]
        [(i:select)
         (>>= (pop 3)
              (λ (a b c)
                (>>= (is-zero c)
                     (λ (zero?) (push (if zero? a b))))))]
        [(i:cover n)
         (>>= (pop)
              (λ (x)
                (let loop ([n n])
                  (if (zero? n)
                    (push x)
                    (>>= (pop)
                         (λ (x)
                           (>> (loop (sub1 n))
                               (push x))))))))]
        [(i:uncover n)
         (>>= (let loop ([n n])
                (if (zero? n)
                  (pop)
                  (>>= (pop)
                       (λ (x)
                         (>>= (loop (sub1 n))
                              (λ (y)
                                (>> (push x)
                                    (unit y))))))))
              push)]
        [(i:concat)
         (primitive-apply concat 2)]
        [(i:substring [start s] [end e])
         (>>= (>>= (pop) (λ (a) (substring a s e))) push)]
        [(i:substring3)
         (primitive-apply substring 3)]
        [(i:getbit)
         (primitive-apply getbit 2)]
        [(i:setbit)
         (primitive-apply setbit 3)]
        [(i:getbyte)
         (primitive-apply getbyte 2)]
        [(i:setbyte)
         (primitive-apply setbyte 3)]
        [(i:extract [start s] [length ℓ])
         (>>= (>>= (pop) (λ (a) (extract a s ℓ))) push)]
        [(i:extract3)
         (primitive-apply extract 3)]
        [(i:extract_uint16)
         (primitive-apply extract-uint 2 2)]
        [(i:extract_uint32)
         (primitive-apply extract-uint 2 4)]
        [(i:extract_uint64)
         (primitive-apply extract-uint 2 8)]
        [(i:balance)
         (>> (in-mode 'Application "balance")
             (primitive-apply balance 1))]
        [(i:app_opted_in)
         (>> (in-mode 'Application "app_opted_in")
             (primitive-apply app-opted-in 2))]
        [(i:app_local_get)
         (>> (in-mode 'Application "app_local_get")
             (primitive-apply app-local-get 2))]
        [(i:app_local_get_ex)
         (>> (in-mode 'Application "app_local_get_ex")
             (primitive-apply app-local-get-ex 3))]
        [(i:app_global_get)
         (>> (in-mode 'Application "app_global_get")
             (primitive-apply app-global-get 1))]
        [(i:app_global_get_ex)
         (>> (in-mode 'Application "app_global_get_ex")
             (primitive-apply app-global-get-ex 2))]
        [(i:app_local_put)
         (>> (in-mode 'Application "app_local_put")
             (primitive-apply app-local-put 3))]
        [(i:app_global_put)
         (>> (in-mode 'Application "app_global_put")
             (primitive-apply app-global-put 2))]
        [(i:app_local_del)
         (>> (in-mode 'Application "app_local_del")
             (primitive-apply app-local-del 2))]
        [(i:app_global_del)
         (>> (in-mode 'Application "app_global_del")
             (primitive-apply app-global-del 1))]
        [(i:asset_holding_get [field f])
         (>> (in-mode 'Application "asset_holding_get")
             (>>= (pop 2) (λ (a b) (asset-holding-get a b f))))]
        [(i:asset_params_get [field f])
         (>> (in-mode 'Application "asset_params_get")
             (>>= (>>= (pop) (λ (a) (asset-params-get a f))) push))]
        [(i:app_params_get [field f])
         (>> (in-mode 'Application "app_params_get")
             (>>= (>>= (pop) (λ (a) (app-params-get a f))) push))]
        [(i:min_balance)
         (>> (in-mode 'Application "min_balance")
             (primitive-apply min-balance 1))]
        [(i:pushbytes [bytes bs])
         (push bs)]
        [(i:pushint [uint n])
         (push n)]
        [(i:callsub offset)
         (>>= get-pc
              (λ (ret-pc)
                (>> (push-call ret-pc)
                    (jump offset))))]
        [(i:retsub)
         (>>= pop-call goto)]
        [(i:shl)
         (primitive-apply shl 2)]
        [(i:shr)
         (primitive-apply shr 2)]
        [(i:sqrt)
         (primitive-apply sqrt 1)]
        [(i:bitlen)
         (primitive-apply bitlen 1)]
        [(i:exp)
         (>>= (pop 2)
              (λ (a b)
                (>>= (is-zero a)
                     (λ (is-zero-a?)
                       (>>= (is-zero b)
                            (λ (is-zero-b?)
                              (if (and is-zero-a? is-zero-b?)
                                (panic "exp: both ~a and ~a are zero" a b)
                                (>>= (exp a b) push))))))))]
        [(i:expw)
         (>>= (pop 2)
              (λ (a b)
                (>>= (is-zero a)
                     (λ (is-zero-a?)
                       (>>= (is-zero b)
                            (λ (is-zero-b?)
                              (if (and is-zero-a? is-zero-b?)
                                (panic "expw: both ~a and ~a are zero" a b)
                                (>>= (expw a b) push))))))))]
        [(i:b+)
         (primitive-apply b+ 2)]
        [(i:b-)
         (primitive-apply b- 2)]
        [(i:b/)
         (>>= (pop 2)
              (λ (a b)
                (>>= (is-zero b)
                     (λ (is-zero?)
                       (if is-zero?
                         (panic "b/: ~a is 0" b)
                         (>>= (b/ a b) push))))))]
        [(i:b*)
         (primitive-apply b* 2)]
        [(i:b<)
         (primitive-apply b< 2)]
        [(i:b>)
         (>> swap
             (primitive-apply b< 2))]
        [(i:b<=)
         (>> swap
             (primitive-apply b< 2)
             (primitive-apply ! 1))]
        [(i:b>=)
         (>> (primitive-apply b< 2)
             (primitive-apply ! 1))]
        [(i:b==)
         (primitive-apply b== 2)]
        [(i:b!=)
         (>> (primitive-apply b== 2)
             (primitive-apply ! 1))]
        [(i:b%)
         (>>= (pop 2)
              (λ (a b)
                (>>= (is-zero b)
                     (λ (is-zero?)
                       (if is-zero?
                         (panic "b%: ~a is 0" b)
                         (>>= (b% a b) push))))))]
        [(i:b\|)
         (primitive-apply b\| 2)]
        [(i:b&)
         (primitive-apply b& 2)]
        [(i:b^)
         (primitive-apply b^ 2)]
        [(i:b~)
         (primitive-apply b~ 1)]
        [(i:bzero)
         (primitive-apply bzero 1)]
        [(i:log)
         (>> (in-mode 'Application "log")
             (primitive-apply log 1))]
        [(i:itxn_begin)
         (>> (in-mode 'Application "itxn_begin")
             itxn-begin)]
        [(i:itxn_next)
         (>> (in-mode 'Application "itxn_next")
             itxn-next)]
        [(i:itxn_field [field f])
         (>> (in-mode 'Application "itxn_field")
             (primitive-apply itxn-field 1 f))]
        [(i:itxn_submit)
         (>> (in-mode 'Application "itxn_submit")
             itxn-submit)]
        [(i:itxn [field f])
         (>> (in-mode 'Application "itxn")
             (primitive-apply itxn-access 0 f))]
        [(i:itxna [field f] [array-index ai])
         (>> (in-mode 'Application "itxna")
             (primitive-apply itxn-array 0 f ai))]
        [(i:txnas [field f])
         (primitive-apply transaction-array 1 f)]
        [(i:gtxnas [group-index gi] [field f])
         (primitive-apply group-transaction-array 1 gi f)]
        [(i:gtxnsas [field f])
         (>>= (>>= (pop 2) (λ (gi ai) (group-transaction-array gi f ai))) push)]
        [(i:args)
         (>> (in-mode 'Signature "args")
             (primitive-apply args 1))]))))

; step : VM m s => m ()
(define (step vm)
  (match-let* ([(VM [monad+ (Monad+ [monad (and monad (Monad >>=))])]
                    [read-byte rb]
                    check-final) vm]
               [>> (>> monad)])
    (>> (>>= (i:read-instruction rb)
             (λ (instr)
               (>> ((logic-sig-version>= vm)
                    (i:instruction-logic-signature-version instr)
                    (i:instruction-name instr))
                   ((execute vm) instr))))
        check-final)))
|#

|#


(define vm0
  (inc (execute
        read-instruction instruction-logic-signature-version instruction-name
        panic check-final
        logic-sig-version
        unit >>= >>)
       [logic-sig-version>=
        (λ (target-lsv info)
          (>>= logic-sig-version
               (λ (lsv)
                 (if (>= lsv target-lsv)
                   (unit)
                   (panic "LogicSig version = ~a but need >= ~a for ~a" lsv target-lsv info)))))]
       [step
        (>> (>>= read-instruction
                 (λ (instr)
                   (>> (logic-sig-version>=
                        (instruction-logic-signature-version instr)
                        (instruction-name instr))
                       (execute instr))))
            check-final)]))

(define vm1
  (inc (group-transaction
        transaction global
        in-mode
        is-zero
        pop push swap
        store
        arg
        lookup-bytecblock put-bytecblock
        lookup-intcblock put-intcblock
        mulw
        ed25519verify sha512-256 keccak256 sha256
        u
        btoi itob len
        ! \|\| &&
        primitive-apply    
        jump continue
        panic
        unit >>= >>)
       [execute
        (sumtype-case-lambda i:Instruction1
          [(i:err)
           (panic "err")]
          [(i:sha256)
           (primitive-apply sha256 1)]
          [(i:keccak256)
           (primitive-apply keccak256 1)]
          [(i:sha512_256)
           (primitive-apply sha512-256 1)]
          [(i:ed25519verify)
           (primitive-apply ed25519verify 3)]
          [(i:+)
           (primitive-apply (u '+) 2)]
          [(i:-)
           (primitive-apply (u '-) 2)]
          [(i:/)
           (>>= (pop 2)
                (λ (a b)
                  (>>= (is-zero b)
                       (λ (zero?)
                         (if zero?
                           (panic "/: ~a is 0" b)
                           (>>= ((u '/) a b) push))))))]
          [(i:*)
           (primitive-apply (u '*) 2)]
          [(i:<)
           (primitive-apply (u '<) 2)]
          [(i:>)
           (>> swap
               (primitive-apply (u '<) 2))]
          [(i:<=)
           (>> swap
               (primitive-apply (u '<) 2)
               (primitive-apply ! 1))]
          [(i:>=)
           (>> (primitive-apply (u '<) 2)
               (primitive-apply ! 1))]
          [(i:&&)
           (primitive-apply && 2)]
          [(i:\|\|)
           (primitive-apply \|\| 2)]
          [(i:==)
           (primitive-apply (u '==) 2)]
          [(i:!=)
           (>> (primitive-apply (u '==) 2)
               (primitive-apply (u '!) 1))]
          [(i:!)
           (primitive-apply ! 1)]
          [(i:len)
           (primitive-apply len 1)]
          [(i:itob)
           (primitive-apply itob 1)]
          [(i:btoi)
           (primitive-apply btoi 1)]
          [(i:%)
           (>>= (pop 2)
                (λ (a b)
                  (>>= (is-zero b)
                       (λ (zero?)
                         (if zero?
                           (panic "%: ~a is 0" b)
                           (>>= ((u '%) a b) push))))))]
          [(i:\|)
           (primitive-apply (u '\|) 2)]
          [(i:&)
           (primitive-apply (u '&) 2)]
          [(i:^)
           (primitive-apply (u '^) 2)]
          [(i:~)
           (primitive-apply (u '~) 1)]
          [(i:mulw)
           (primitive-apply mulw 2)]
          [(i:intcblock [uints ns])
           (put-intcblock ns)]
          [(i:intc i)
           (>>= (lookup-intcblock i) push)]
          [(i:intc_0)
           (>>= (lookup-intcblock 0) push)]
          [(i:intc_1)
           (>>= (lookup-intcblock 1) push)]
          [(i:intc_2)
           (>>= (lookup-intcblock 2) push)]
          [(i:intc_3)
           (>>= (lookup-intcblock 3) push)]
          [(i:bytecblock [bytess bss])
           (put-bytecblock bss)]
          [(i:bytec i)
           (>>= (lookup-bytecblock i) push)]
          [(i:bytec_0)
           (>>= (lookup-bytecblock 0) push)]
          [(i:bytec_1)
           (>>= (lookup-bytecblock 1) push)]
          [(i:bytec_2)
           (>>= (lookup-bytecblock 2) push)]
          [(i:bytec_3)
           (>>= (lookup-bytecblock 3) push)]
          [(i:arg n)
           (>> (in-mode 'Signature "arg")
               (primitive-apply arg 0 n))]
          [(i:arg_0)
           (>> (in-mode 'Signature "arg_0")
               (primitive-apply arg 0 0))]
          [(i:arg_1)
           (>> (in-mode 'Signature "arg_1")
               (primitive-apply arg 0 1))]
          [(i:arg_2)
           (>> (in-mode 'Signature "arg_2")
               (primitive-apply arg 0 2))]
          [(i:arg_3)
           (>> (in-mode 'Signature "arg_3")
               (primitive-apply arg 0 3))]
          [(i:txn [field f])
           (primitive-apply transaction 0 f)]
          [(i:global [field f])
           (primitive-apply global 0 f)]
          [(i:gtxn [group-index gi] [field f])
           (primitive-apply group-transaction 0 gi f)]
          [(i:load i)
           (primitive-apply load 0 i)]
          [(i:store i)
           (primitive-apply store 1 i)]
          [(i:bnz offset)
           (>>= (>>= (pop) is-zero)
                (λ (stay?)
                  (if stay?
                    continue
                    (jump offset))))]
          [(i:pop)
           (>> (pop)
               continue)]
          [(i:dup)
           (>>= (pop) (λ (x) (push x x)))])]))

(define vm-extras
  (inc (pop
        unit >>= >>)
       [pop
        (λ ([n 1])
          (let loop ([n n]
                     [xs (list)])
            (if (zero? n)
              (apply unit xs)
              (>>= (super pop) (λ (x) (loop (sub1 n) (cons x xs)))))))]
       [push
        (λ xs (foldr (λ (x m) (>> ((super push) x) m)) (unit) xs))]
       [primitive-apply
        (λ (f stack-arity . xs)
          (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))]
))


#|
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
      (>>= (logic-sig-version lsv) 
           (λ (lsv)
             (if (and (< offset 0)
                      (< lsv 4))
               (panic "cannot jump backwards (offset ~a) in LogicSigVersion ~a < 4" offset lsv)
               (>>= get-pc (λ (pc) (goto (+ pc offset))))))))
    (define (goto pc)
      (if (< pc 0)
        (panic "cannot go to negative counter ~a" pc)
        (>>= get-bytecode
             (λ (bc)
               (if (> pc (bytes-length bc))
                 (panic "cannot go to ~a past bytecode of length ~a" pc (bytes-length bc))
                 (>>= (logic-sig-version lsv) 
                      (λ (lsv)
                        (if (and (= pc (bytes-length bc))
                                 (< lsv 2))
                          (panic "cannot go to end of bytecode (length ~a) in version ~a < 2" (bytes-length bc) lsv)
                          (set-pc pc)))))))))

(define swap (>>= (pop 2) (λ (a b) (push b a))))
    (define (primitive-apply f stack-arity . xs)
      (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))
    (define-syntax-rule (machine-fail instr)
      (error 'vm "unimplemented instruction ~a" 'instr))


|#

(define (vm/version v)
  (if (and (exact-nonnegative-integer? v)
           (<= 1 v 6))
    (apply mix
           vm-extras
           (reverse (take (list vm0 vm1 ;vm2 vm3 vm4 vm5 #;vm6
                                ) (add1 v))))
    (error 'vm/version "expected version 1, 2, 3, 4, 5, or 6; received ~v" v)))

(provide vm/version)

#|

(define step
  (mix (execute
        check-final
        logic-sig-version>=
        read-instruction
        >>= >>)
       [step
        (>> (>>= read-instruction
                 (λ (instr)
                   (>> (logic-sig-version>=
                        (i:instruction-logic-signature-version instr)
                        (i:instruction-name instr))
                       (execute instr))))
            check-final)]))



; logic-sig-version>= : VM m s => integer string -> m ()
(define (logic-sig-version>= vm)
  (match-let ([(VM0 [monad+ (Monad+ unit >>=)]
                    [logic-sig-version lsv]
                    panic)
               vm])
    )
  )
#|
(provide VM
         step
         logic-sig-version>=)
|#
|#
