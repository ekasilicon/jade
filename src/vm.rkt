#lang racket/base
(require (only-in racket/list take)
         "static/object.rkt"
         "static/sumtype.rkt"
         (prefix-in i: "instruction.rkt"))

(define vm0
  (inc (execute
        read-instruction instruction-logic-signature-version instruction-name
        get-intcblock get-bytecblock
        constant
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
            check-final)]
       [lookup-intcblock
        (λ (i)
          (>>= get-intcblock
           (λ (xs)
             (if (< i (length xs))
               (constant (list-ref xs i))
               (panic "intcblock has ~a ints but index ~a requested" (length xs) i)))))]
       [lookup-bytecblock
        (λ (i)
          (>>= get-bytecblock
               (λ (bss)
                 (if (< i (length bss))
                   (constant (list-ref bss i))
                   (panic "bytecblock has ~a bytes but index ~a requested" (length bss) i)))))]))

(define vm1
  (inc (group-transaction
        transaction global
        in-mode
        is-zero
        pop push swap
        store load
        arg
        put-intcblock lookup-intcblock
        put-bytecblock lookup-bytecblock
        mulw
        ed25519verify sha512-256 keccak256 sha256
        u~ u^ u& u\| u% u== u< u* u/ u- u+
        btoi itob len
        ! \|\| &&
        primitive-apply
        get-bytecode
        get-pc set-pc
        logic-sig-version
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
                (λ (stay?) (if stay? continue (jump offset))))]
          [(i:pop)
           (>> (pop)
               continue)]
          [(i:dup)
           (>>= (pop) (λ (x) (push x x)))])]
       [continue
        (unit)]
       [jump
        (λ (offset)
          (if (< offset 0)
            (>>= logic-sig-version
                 (λ (lsv) (panic "cannot jump backwards (offset ~a) in LogicSigVersion ~a" offset lsv)))
            (>>= get-pc (λ (pc) (goto (+ pc offset))))))]
       [goto
        (λ (pc)
          (if (< pc 0)
            (panic "cannot go to negative counter ~a" pc)
            (>>= get-bytecode
                 (λ (bc)
                   (if (>= pc (bytes-length bc))
                     (panic "cannot go to ~a at or past end of bytecode of length ~a" pc (bytes-length bc))
                     (set-pc pc))))))]))



(define vm2
  (inc (asset-params-get
        asset-holding-get
        concat substring
        balance
        addw
        app-opted-in
        transaction-array group-transaction-array
        app-local-get app-local-get-ex app-local-put app-local-del
        app-global-get app-global-get-ex app-global-put app-global-del
        get-bytecode
        get-pc set-pc
        push pop
        primitive-apply
        in-mode
        panic
        continue jump
        is-zero
        return
        unit >>= >>)
       [execute
        (sumtype-case-lambda i:Instruction2
          [(i:Instruction1 instr)
           ((super execute) instr)]          
          [(i:addw)
           (primitive-apply addw 2)]
          [(i:txna [field f] [array-index ai])
           (primitive-apply transaction-array 0 f ai)]
          [(i:gtxna [group-index gi] [field f] [array-index ai])
           (primitive-apply group-transaction-array 0 gi f ai)]
          [(i:bz offset)
           (>>= (>>= (pop) is-zero)
                (λ (jump?) (if jump? (jump offset) continue)))]
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
               (>>= (>>= (pop) (λ (a) (asset-params-get a f))) push))])]
       [goto
        (λ (pc)
          (if (< pc 0)
            (panic "cannot go to negative counter ~a" pc)
            (>>= get-bytecode
                 (λ (bc)
                   (if (> pc (bytes-length bc))
                     (panic "cannot go to ~a past end of bytecode of length ~a" pc (bytes-length bc))
                     (set-pc pc))))))]))

(define vm3
  (inc (min-balance
        getbit setbit
        getbyte setbyte
        group-transaction group-transaction-array
        is-zero
        pop push swap
        jump continue
        primitive-apply
        in-mode
        constant
        panic
        unit >>= >>)
       [execute
        (sumtype-case-lambda i:Instruction3
          [(i:Instruction2 instr)
           ((super execute) instr)]          
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
           (primitive-apply constant 0 bs)]
          [(i:pushint [uint n])
           (primitive-apply constant 0 n)])]))

(define vm4
  (λ (self super)
    (λ (msg)
      (error 'vm4 "implement vm4 for ~a" msg)))
  #;
  (inc (get-pc
        goto
        >>=)
       [jump
        (λ (offset) (>>= get-pc (λ (pc) (goto (+ pc offset)))))]))

(define vm5
  (λ (self super)
    (λ (msg)
      (error 'vm5 "implement vm5 for ~a" msg))))

(define vm6
  (λ (self super)
    (λ (msg)
      (error 'vm6 "implement vm6 for ~a" msg))))

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
       [swap
        (>>= (pop 2) (λ (a b) (push b a)))]
       [primitive-apply
        (λ (f stack-arity . xs)
          (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))]))

(require "version.rkt")

(define vm/version
  (make-*/version 'vm/version vm0 vm1 vm2 vm3 vm4 vm5 vm6 vm-extras))

(provide vm/version)
