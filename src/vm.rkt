#lang racket/base
(require (only-in racket/match match-lambda)
         (only-in racket/list take)
         "static/object.rkt"
         "static/sumtype.rkt"
         (prefix-in i: "instruction.rkt")
         "assembly.rkt")

(define vm0
  (inc (execute
        read-instruction instruction-version instruction-name
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
                   (>> (check-version! instr) 
                       (execute instr))))
            check-final)]
       [check-version!
        (λ (instr)
          (logic-sig-version>= (instruction-version instr) (instruction-name instr)))]))

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
        jump
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
               (>>= (arg n) push))]
          [(i:arg_0)
           (>> (in-mode 'Signature "arg_0")
               (>>= (arg 0) push)
               (primitive-apply arg 0 0))]
          [(i:arg_1)
           (>> (in-mode 'Signature "arg_1")
               (>>= (arg 1) push))]
          [(i:arg_2)
           (>> (in-mode 'Signature "arg_2")
               (>>= (arg 2) push))]
          [(i:arg_3)
           (>> (in-mode 'Signature "arg_3")
               (>>= (arg 3) push))]
          [(i:txn field)
           (>>= (transaction field) push)]
          [(i:global field)
           (>>= (global field) push)]
          [(i:gtxn group-index field)
           (>>= (group-transaction group-index field) push)]
          [(i:load i)
           (>>= (load i) push)]
          [(i:store i)
           (>>= (>>= (pop) (λ (x) (store i x))) push)]
          [(i:bnz offset)
           (>>= (>>= (pop) is-zero)
                (λ (stay?) (if stay? continue (jump offset))))]
          [(i:pop)
           (>> (pop)
               continue)]
          [(i:dup)
           (>>= (pop) (λ (x) (push x x)))])]
       [continue
        (unit)]))



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
        ;get-bytecode
        ;get-pc set-pc
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
           ((super 'execute) instr)]          
          [(i:addw)
           (primitive-apply addw 2)]
          [(i:txna field array-index)
           (>>= (transaction-array field array-index) push)]
          [(i:gtxna group-index field array-index)
           (>>= (group-transaction-array group-index field array-index) push)]
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
               (>>= (>>= (pop 2) (λ (a b) (asset-holding-get a b f))) push))]
          [(i:asset_params_get [field f])
           (>> (in-mode 'Application "asset_params_get")
               (>>= (>>= (pop) (λ (a) (asset-params-get a f))) push))])]
       #;
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
           ((super 'execute) instr)]          
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
                                  (>> (push x)
                                      (unit y))))))))
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
  (inc (>>= >>
        in-mode
        pop
        primitive-apply
        gload
        gaid
        divmodw
        callsub retsub
        usqrt
        swap !
        b+ b- b/ b* b< b> b<= b>= b== b!= b% b\| b& b^ b~ bzero
        shl shr bitlen exp expw)
       [execute
        (sumtype-case-lambda i:Instruction4
          [(i:Instruction3 instr)
           ((super 'execute) instr)]
          [(i:gload group-index i)
           (>> (in-mode 'Application "gload")
               (primitive-apply gload 0 group-index i))]
          [(i:gloads i)
           (>> (in-mode 'Application "gloads")
               (>>= (pop) (λ (group-index) (primitive-apply gload 0 group-index i))))]
          [(i:gaid group-index)
           (>> (in-mode 'Application "gaid")
               (primitive-apply gaid 0 group-index))]
          [(i:gaids)
           (>> (in-mode 'Application "gaids")
               (primitive-apply gaid 1))]
          [(i:divmodw)
           (primitive-apply divmodw 4)]
          [(i:callsub offset)
           (primitive-apply callsub 0 offset)]
          [(i:retsub)
           retsub]
          [(i:sqrt)
           (primitive-apply usqrt 1)]
          [(i:b+)
           (primitive-apply b+ 2)]
          [(i:b-)
           (primitive-apply b- 2)]
          [(i:b/)
           (primitive-apply b/ 2)]
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
           (primitive-apply b% 2)]
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
          [(i:shl)
           (primitive-apply shl 2)]
          [(i:shr)
           (primitive-apply shr 2)]
          [(i:bitlen)
           (primitive-apply bitlen 1)]
          [(i:exp)
           (primitive-apply exp 2)]
          [(i:expw)
           (primitive-apply expw 2)])]))

(define vm5
  (inc (unit >>= >>
        in-mode
        pop push
        primitive-apply
        log
        ecdsa-verify ecdsa-pk-decompress ecdsa-pk-recover
        load store
        extract
        extract-uint
        app-params-get
        transaction-array group-transaction-array
        inner-transaction-begin inner-transaction-field inner-transaction-submit inner-transaction inner-transaction-array
        arg)
       [execute
        (sumtype-case-lambda i:Instruction5
          [(i:Instruction4 instr)
           ((super 'execute) instr)]
          [(i:log)
           (>> (in-mode 'Application "log")
               (primitive-apply log 1))]
          [(i:loads)
           (primitive-apply load 1)]
          [(i:stores)
           (primitive-apply store 2)]
          [(i:extract start length)
           (>>= (>>= (pop) (λ (bs) (extract bs start length))) push)]
          [(i:extract3)
           (primitive-apply extract 3)]
          [(i:extract_uint16)
           (primitive-apply extract-uint 2 2)]
          [(i:extract_uint32)
           (primitive-apply extract-uint 2 4)]
          [(i:extract_uint64)
           (primitive-apply extract-uint 2 8)]
          [(i:app_params_get field)
           (>> (in-mode 'Application "app_params_get")
               (primitive-apply app-params-get 1 field))]
          [(i:ecdsa_verify v)
           (primitive-apply ecdsa-verify 5 v)]
          [(i:ecdsa_pk_decompress v)
           (primitive-apply ecdsa-pk-decompress 1 v)]
          [(i:ecdsa_pk_recover v)
           (primitive-apply ecdsa-pk-recover 4 v)]
          [(i:itxn_begin)
           (>> (in-mode 'Application "itxn_begin")
               inner-transaction-begin)]
          [(i:itxn_field [field f])
           (>> (in-mode 'Application "itxn_field")
               (>>= (>>= (pop) (λ (v) (inner-transaction-field f v))) push))]
          [(i:itxn_submit)
           (>> (in-mode 'Application "itxn_submit")
               inner-transaction-submit)]
          [(i:itxn field)
           (>> (in-mode 'Application "itxn")
               (>>= (inner-transaction field) push))]
          [(i:itxna [field f] [array-index ai])
           (>> (in-mode 'Application "itxna")
               (>>= (inner-transaction-array f ai) push))]
          [(i:txnas [field f])
           (>>= (>>= (pop) (λ (ai) (transaction-array f ai))) push)]
          [(i:gtxnas [group-index gi] [field f])
           (>>= (>>= (pop) (λ (ai) (group-transaction-array gi f ai))) push)]
          [(i:gtxnsas [field f])
           (>>= (>>= (pop 2) (λ (gi ai) (group-transaction-array gi f ai))) push)]
          [(i:args)
           (>> (in-mode 'Signature "args")
               (>>= (>>= (pop) arg) push))]
          [(i:cover n)
           (>>= (pop)
                (λ (x)
                  (let loop ([n n])
                    (if (zero? n)
                      (push x)
                      (>>= (pop)
                           (λ (y)
                             (>> (loop (sub1 n))
                                 (push y))))))))]
          [(i:uncover n)
           (>>= (let loop ([n n])
                  (if (zero? n)
                    (pop)
                    (>>= (pop)
                         (λ (y)
                           (>>= (loop (sub1 n))
                                (λ (x)
                                  (>> (push y)
                                      (unit x))))))))
                push)])]))

(define vm6
  (inc (>> >>=
        push pop
        in-mode
        primitive-apply
        bsqrt
        divw
        inner-transaction-next group-inner-transaction group-inner-transaction-array inner-transaction-array
        gload
        acct-params-get)
       [execute
        (sumtype-case-lambda i:Instruction6
          [(i:Instruction5 instr)
           ((super 'execute) instr)]
          [(i:bsqrt)
           (primitive-apply bsqrt 1)]
          [(i:divw)
           (primitive-apply divw 3)]
          [(i:itxn_next)
           (>> (in-mode 'Application "itxn_next")
               inner-transaction-next)]
          [(i:gitxn [group-index gi] [field f])
           (>> (in-mode 'Application "gitxn")
               (>>= (group-inner-transaction gi f) push))]
          [(i:gitxna [group-index gi] [field f] [array-index ai])
           (>> (in-mode 'Application "gitxna")
               (>>= (group-inner-transaction-array gi f ai) push))]
          [(i:itxnas [field f])
           (>>= (>>= (pop) (λ (ai) (inner-transaction-array f ai))) push)]
          [(i:gitxnas [group-index gi] [field f])
           (>>= (>>= (pop) (λ (ai) (group-inner-transaction-array gi f ai))) push)]
          [(i:gloadss)
           (>> (in-mode 'Application "gloadss")
               (primitive-apply gload 2))]
          [(i:acct_params_get field)
           (>> (in-mode 'Application "acct_params_get")
               (primitive-apply acct-params-get 1 field))])]))

(define vm7
  (inc (>>= >> pop push
        in-mode
        primitive-apply
        replace base64-decode json-ref ed25519verify-bare sha3-256 vrf-verify block)
       [execute
        (sumtype-case-lambda i:Instruction7
          [(i:Instruction6 instr)
           ((super 'execute) instr)]
          [(i:replace2 s)
           (>>= (>>= (pop 2) (λ (A B) (replace A s B))) push)]
          [(i:replace3)
           (primitive-apply replace 3)]
          [(i:base64_decode encoding)
           (primitive-apply base64-decode 1 encoding)]
          [(i:json_ref type)
           (primitive-apply json-ref 2 type)]
          [(i:ed25519verify_bare)
           (primitive-apply ed25519verify-bare 3)]
          [(i:sha3_256)
           (primitive-apply sha3-256 1)]
          [(i:vrf_verify standard)
           (primitive-apply vrf-verify 3 standard)]
          [(i:block field)
           (primitive-apply block 1 field)])]))

(define vm8
  (inc (>>= pop switch)
       [execute
        (sumtype-case-lambda i:Instruction8
          [(i:Instruction7 instr)
           ((super 'execute) instr)]
          [(i:switch offsets)
           (>>= (pop) (λ (i) (switch offsets i)))])]))

(define vm-extras
  (inc (pop
        unit >>= >>)
       [pop
        (λ ([n 1])
          (let loop ([n n]
                     [xs (list)])
            (if (zero? n)
              (apply unit xs)
              (>>= (super 'pop) (λ (x) (loop (sub1 n) (cons x xs)))))))]
       [push
        (λ xs (foldr (λ (x m) (>> ((super 'push) x) m)) (unit) xs))]
       [swap
        (>>= (pop 2) (λ (a b) (push b a)))]
       [primitive-apply
        (λ (f stack-arity . xs)
          (>>= (>>= (pop stack-arity) (λ ys (apply f (append xs ys)))) push))]))

(define vm-pseudo
  (inc (unit push)
       [execute
        (sumtype-case-lambda Pseudoinstruction
          [(varuint-immediate value)
           (push value)]
          [(bytes-immediate value)
           (push value)]
          [(instruction [instruction instr])
           ((super 'execute) instr)])]
       [check-version!
        (sumtype-case-lambda Pseudoinstruction
          [(varuint-immediate)
           (unit)]
          [(bytes-immediate)
           (unit)]
          [(instruction [instruction instr])
           ((super 'check-version!) instr)])]))

(require "version.rkt")

(define vm/version
  (make-*/version 'vm/version vm0 vm1 vm2 vm3 vm4 vm5 vm6 vm7 vm8 vm-extras))

(provide vm/version
         vm-pseudo)
