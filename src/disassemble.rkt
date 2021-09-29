#lang racket/base
(require racket/match
         racket/set
         racket/port
         "../record.rkt"
         "../monad.rkt"
         "../read-byte.rkt")

(record failure (message))
(record success (values state index))

(define ((unit . xs) bs s i)
  (success [values xs]
           [state s]
           [index i]))
(define ((>>= m f) bs s i)
  (match (m bs s i)
    [(success [values xs] [state s] [index i])
     ((apply f xs) bs s i)]
    [(failure message)
     (failure message)]
    [#f
     #f]))
(define (>> m . ms) (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define ((mplus m₀ m₁) bs s i)
  (cond
    [(m₀ bs s i) => values]
    [else (m₁ bs s i)]))

(define ((fail template . args) bs s i)
  (failure [message (apply format template args)]))

(define Disassemble-ReadByte
  (ReadByte [Monad (Monad unit >>= >>)]
            [read-byte (λ (bs s i)
                         (if (= (bytes-length bs) i)
                           #f
                           (success [values (list (bytes-ref bs i))]
                                    [state s]
                                    [index (add1 i)])))]))

(define rb Disassemble-ReadByte)

(define ((lift f) . xs)
  (call-with-values (λ () (apply f xs)) unit))

(define (stream-end bs s i)
  (if (= (bytes-length bs) i)
    (success [values (list)] [state s] [index i])
    #f))

(define (position bs s i) (success [values (list i)] [state s] [index i]))

(define ((mget k d) bs s i)
  (success [values (list (hash-ref s k d))] [state s] [index i]))
(define (((mset k) v) bs s i)
  (success [values (list)] [state (hash-set s k v)] [index i]))
(define ((mupdate k f d) bs s i)
  (success [values (list)] [state (hash-update s k f d)] [index i]))

(define (push-destination n)
  (mupdate 'destinations (λ (ns) (cons n ns)) (list)))

(define transaction-field
  (>>= (read-uint8 rb) 
       (lift
        (match-lambda
          [0  'Sender]
          [1  'Fee]
          [6  'Lease]
          [7  'Receiver]
          [8  'Amount]
          [9  'CloseRemainderTo]
          [16 'TypeEnum]
          [17 'XferAsset]
          [18 'AssetAmount]
          [20 'AssetReceiver]
          [21 'AssetCloseTo]
          [22 'GroupIndex]
          [24 'ApplicationID]
          [25 'OnCompletion]
          [26 'ApplicationArgs]
          [27 'NumAppArgs]
          [32 'RekeyTo]))))

(define global-field
  (>>= (read-uint8 rb) 
       (lift
        (match-lambda
          [3  'ZeroAddress]
          [4  'GroupSize]
          [6  'Round]
          [7  'LatestTimestamp]
          [9  'CreatorAddress]))))

(define destination
  (>>= (read-int16 rb) 
       (λ (offset)
         (>>= position
              (λ (i)
                (let ([dst (+ i offset)])
                  (>> (push-destination dst)
                      (unit dst))))))))

; Disassemble Instruction where instance Monad Disassemble
(define disassemble-instruction
  (>>= (read-opcode rb) 
       (match-lambda
         [#x00 (unit `(err))]
         [#x01 (unit `(sha256))]
         [#x02 (unit `(keccak256))]
         [#x03 (unit `(sha512_256))]
         [#x04 (unit `(ed25519verify))]
         [#x05 (>>= (read-uint8 rb) (λ (v) (unit `(ecdsa_verify ,v))))]
         [#x06 (>>= (read-uint8 rb) (λ (v) (unit `(ecdsa_pk_decompress ,v))))]
         [#x07 (>>= (read-uint8 rb) (λ (v) (unit `(ecdsa_pk_recover ,v))))]
         [#x08 (unit `(+))]
         [#x09 (unit `(-))]
         [#x0a (unit `(/))]
         [#x0b (unit `(*))]
         [#x0c (unit `(<))]
         [#x0d (unit `(>))]
         [#x0e (unit `(<=))]
         [#x0f (unit `(>=))]
         [#x10 (unit `(&&))]
         [#x11 (unit `(\|\|))]
         [#x12 (unit `(==))]
         [#x13 (unit `(!=))]
         [#x14 (unit `(!))]
         [#x15 (unit `(len))]
         [#x16 (unit `(itob))]
         [#x17 (unit `(btoi))]
         [#x18 (unit `(%))]
         [#x1a (unit `(&))]
         [#x20 (>>= (read-intcblock rb) (λ (ns) (unit `(intcblock . ,ns))))]
         [#x21 (>>= (read-uint8 rb) (λ (i) (unit `(intc ,i))))]
         [#x22 (unit `(intc_0))]
         [#x23 (unit `(intc_1))]
         [#x24 (unit `(intc_2))]
         [#x25 (unit `(intc_3))]
         [#x26 (>>= (read-bytecblock rb) (λ (bss) (unit `(bytecblock . ,bss))))]
         [#x27 (>>= (read-uint8 rb) (λ (i) (unit `(bytec ,i))))]
         [#x28 (unit `(bytec_0))]
         [#x29 (unit `(bytec_1))]
         [#x2a (unit `(bytec_2))]
         [#x2b (unit `(bytec_3))]
         [#x31 (>>= transaction-field (λ (f) (unit `(txn ,f))))]
         [#x32 (>>= global-field (λ (f) (unit `(global ,f))))]
         [#x34 (>>= (read-uint8 rb) (λ (i) (unit `(load ,i))))]
         [#x35 (>>= (read-uint8 rb) (λ (i) (unit `(store ,i))))]
         [#x36
          (>>= transaction-field 
               (λ (f)
                 (>>= (read-uint8 rb) 
                      (λ (ai)
                        (unit `(txna ,f ,ai))))))]
         [#x38 (>>= transaction-field  (λ (f) (unit `(gtxns ,f))))]
         [#x40 (>>= destination (λ (dst) (unit `(bnz ,dst))))]
         [#x41 (>>= destination (λ (dst) (unit `(bz ,dst))))]
         [#x42 (>>= destination (λ (dst) (unit `(b ,dst))))]
         [#x43 (unit `(return))]
         [#x44 (unit `(assert))]
         ; #x45-#x47 unused
         [#x48 (unit `(pop))]
         [#x49 (unit `(dup))]
         [#x4a (unit `(dup2))]
         [#x4b (>>= (read-uint8 rb) (λ (n) (unit `(dig ,n))))]
         [#x4c (unit `(swap))]
         [#x4d (unit `(select))]
         [#x4e (>>= (read-uint8 rb) (λ (n) (unit `(cover ,n))))]
         [#x4f (>>= (read-uint8 rb) (λ (n) (unit `(uncover ,n))))]
         [#x50 (unit `(concat))]
         [#x51 (>>= (read-uint8 rb) (λ (s) (>>= (read-uint8 rb) (λ (e) (unit `(substring ,s ,e))))))]
         [#x52 (unit `(substring3))]
         [#x62 (unit `(app_local_get))]
         [#x64 (unit `(app_global_get))]
         [#x65 (unit `(app_global_get_ex))]
         [#x66 (unit `(app_local_put))]
         [#x67 (unit `(app_global_put))]
         [#x68 (unit `(app_local_del))]
         [#x69 (unit `(app_global_del))]
         [#x80 (>>= (read-bytes rb) (λ (bs) (unit `(pushbytes ,bs))))]
         [#x81 (>>= (read-varuint rb) (λ (n) (unit `(pushint ,n))))]
         [#x88 (>>= destination (λ (dst) (unit `(callsub ,dst))))]
         [#x89 (unit `(retsub))]
         [#xaa (unit `(b%))]
         [#xab (unit `(b\|))]
         [#xac (unit `(b&))]
         [#xad (unit `(b^))]
         [#xae (unit `(b~))]
         [#xaf (unit `(bzero))]
         [#xb0 (unit `(log))]
         [#xb1 (unit `(itxn_begin))]
         [#xb2 (>>= transaction-field (λ (f) (unit `(itxn_field ,f))))]
         [#xb3 (unit `(itxn_submit))]
         [#xb4 (>>= transaction-field (λ (f) (unit `(itxn ,f))))]
         [#xb5 (>>= transaction-field (λ (f) (>>= (read-uint8 rb)  (λ (i) (unit `(itxna ,f ,i))))))]
         ; #xb6-#xbf unused
         [#xc0 (>>= transaction-field (λ (f) (unit `(txnas ,f))))])))

(define disassemble-instruction-stream
  (>>= position
       (λ (lft)
         (mplus (>> stream-end
                    (mupdate 'instructions (λ (h) (hash-set h lft `(done))) (hasheqv)))
                (>> (>>= disassemble-instruction
                         (λ (instr)
                           (>>= position
                                (λ (rgt)
                                  (mupdate 'instructions (λ (h) (hash-set h lft `(succ ,instr ,rgt))) (hasheqv))))))
                    disassemble-instruction-stream)))))

(define disassemble
  (>> (mplus (>>= (read-varuint rb) (mset 'logic-sig-version))
             (fail "unable to read initial logic signature version sequence"))
      (>>= position (mset 'initial))
      disassemble-instruction-stream))

(define (run bs)
  (disassemble bs (hasheq) 0))

(define (disassemble-bytes bs)
  (match (run bs)
    [(success [values (list)] state index)
     (state→assembly state)]
    [(failure message)
     (error 'disassemble message)]
    [#f
     (error 'disassemble "internal error")]))

(define (disassemble-port ip)
  (disassemble-bytes (port->bytes ip)))

(define (state→assembly state)
  (cons `(pragma ,(format "version ~a" (hash-ref state 'logic-sig-version)))
        (let ([instrs (hash-ref state 'instructions)]
              [start-i (hash-ref state 'initial)])
          (define (instr-ref i)
            (cond
              [(hash-ref instrs i #f)
               => values]
              [else
               (error 'disassemble "expected instruction at byte index ~a" i)]))
          (let* ([dsts (let loop ([i start-i]
                                  [dsts (set)])
                         (match (instr-ref i)
                           [`(done)
                            dsts]
                           [`(succ ,instr ,next-i)
                            (loop next-i
                                  (match instr
                                    [(or `(b ,dst)
                                         `(bz ,dst)
                                         `(bnz ,dst)
                                         `(callsub ,dst))
                                     (if (hash-has-key? instrs dst)
                                       (set-add dsts dst)
                                       (error 'disassemble "branch to byte index ~a not on instruction boundary" dst))]
                                    [_
                                     dsts]))]))]
                 [dsts (for/hash ([dst (in-list (sort (set->list dsts) <))]
                                  [i (in-naturals)])
                         (values dst (string->symbol (format "label~a" i))))])
            (let loop ([i start-i])
              (let ([instrs (match (instr-ref i)
                              [`(done)
                               (list)]
                              [`(succ ,instr ,next-i)
                               (cons (match instr
                                       [(list (and code (or 'b 'bz 'bnz 'callsub)) dst)
                                        (list code (hash-ref dsts dst))]
                                       [_
                                        instr])
                                     (loop next-i))])])
                (cond
                  [(hash-ref dsts i #f)
                   => (λ (ℓ) (cons `(label ,ℓ) instrs))]
                  [else instrs])))))))

(define (state→AST state)
  (let ([instructions (hash-ref state 'instructions)]
        [start-i (hash-ref state 'initial)])
    ; instructions is a map from a left boundary to a `(succ ,instr ,next-i)
    ; where instr is the instruction at that boundary and next-i is the right boundary
    ; start-i is the left boundary of the first instruction

    ; this loop does a pass through the instructions
    ; to create a map from left boundaries to placeholders
    ; which contain the instruction sequence (itself indirected by internal placeholders)
    (let ([phs (let loop ([i start-i]
                          [phs (hash-set (hasheqv) start-i (make-placeholder #f))])
                 (match (hash-ref instructions i)
                   [`(succ ,instr ,next-i)
                    (let ([ph (make-placeholder #f)])
                      (placeholder-set! (hash-ref phs i) (cons instr ph))
                      (loop next-i (hash-set phs next-i ph)))]
                   [`(done)
                    (placeholder-set! (hash-ref phs i) (list))
                    phs]))])

      ; this loop does a pass to fix instructions whose successor
      ; isn't simply the next instruction
      ; these instructions include terminal instructions,
      ; such as err, return, and retsub,
      ; and branching instructions,
      ; such as bnz, bz, and callsub.
      ; it also inlines b instructions.
      (let loop ([i start-i])
        (match (hash-ref instructions i)
          [`(succ ,instr ,next-i)
           (match instr
             [(list (and code (or 'bnz 'bz 'callsub))
                    pc)
              (cond
                [(hash-ref phs pc #f)
                 => (λ (ph)
                      (placeholder-set! (hash-ref phs i)
                                        (cons (list code ph)
                                              (hash-ref phs next-i))))]
                [else
                 (error 'disassemble "branch to byte offset ~a not on instruction boundary" pc)])]
             [(list 'b pc)
              ; don't set it to the contents of the destination placeholder
              ; because those might be changed by this loop
              (placeholder-set! (hash-ref phs i)
                                (hash-ref phs pc))]
             [(or `(err)
                  `(retsub)
                  `(return))
              (placeholder-set! (hash-ref phs i)
                                (cons instr (list)))]
             [_
              (void)])
           (loop next-i)]
          [`(done)
           (void)]))
      (make-reader-graph (hash-ref phs start-i)))))

(provide disassemble-bytes
         disassemble-port)

(module+ main
  (require racket/pretty)

  (pretty-print (disassemble-port (current-input-port))))
