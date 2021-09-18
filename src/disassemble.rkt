#lang racket/base
(require racket/match)

; ensure that jumps occur on instruction boundaries
; get rid of intcblocks and bytecblocks?

(define ((unit . xs) bs s i) (list* xs s i))
(define ((>>= m f) bs s i)
  (match (m bs s i)
    [(list* xs s i)
     ((apply f xs) bs s i)]
    [#f
     #f]))

(define (>> m . ms)
  (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define ((∨ m₀ m₁) bs s i)
  (cond
    [(m₀ bs s i) => values]
    [else (m₁ bs s i)]))

(define ((lift f) . xs)
  (call-with-values (λ () (apply f xs)) unit))

(define (read-byte bs s i)
  (if (= (bytes-length bs) i)
    #f
    (list* (list (bytes-ref bs i)) s (add1 i))))

(define (stream-end bs s i)
  (if (= (bytes-length bs) i)
    (list* (list) s i)
    #f))

(define (position bs s i) (list* (list i) s i))

(define ((get k d) bs s i)
  (list* (list (hash-ref s k d))
         s
         i))
(define ((set k v) bs s i)
  (list* (list)
         (hash-set s k v)
         i))
(define ((update k f d) bs s i)
  (list* (list)
         (hash-update s k f d)
         i))

(define (push-destination n)
  (update 'destinations (λ (ns) (cons n ns)) (list)))

(define read-opcode
  read-byte)

(define read-uint8 read-byte)
(define read-int16
  (>>= read-byte
       (λ (bu)
         (>>= read-byte
              (λ (bl)
                (let ([x (+ (* 256 bu) bl)])
                  (unit (if (< x (expt 2 15))
                          x
                          (- x (expt 2 16))))))))))

(define read-varuint
  (>>= read-byte
       (λ (b)
         (if (zero? (bitwise-and b #x80))
           (unit b)
           (>>= read-varuint (λ (n) (unit (+ (bitwise-and b #x7F) (* n #x80)))))))))

(define read-bytes
  (>>= (>>= read-varuint
            (λ (n)
              (let loop ([n n])
                (if (zero? n)
                  (unit (list))
                  (>>= read-byte
                       (λ (b)
                         (>>= (loop (sub1 n))
                              (λ (bs) (unit (cons b bs))))))))))
       (λ (bs) (unit (apply bytes bs)))))

(define transaction-field
  (>>= read-uint8
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
  (>>= read-uint8
       (lift
        (match-lambda
          [3  'ZeroAddress]
          [4  'GroupSize]
          [6  'Round]
          [7  'LatestTimestamp]
          [9  'CreatorAddress]))))

(define destination
  (>>= read-int16
       (λ (offset)
         (>>= position
              (λ (i)
                (let ([dst (+ i offset)])
                  (>> (push-destination dst)
                      (unit dst))))))))

(define disassemble-instruction
  (>>= read-opcode
       (match-lambda
         [#x00 (unit `(err))]
         [#x01 (unit `(sha256))]
         [#x02 (unit `(keccak256))]
         [#x03 (unit `(sha512_256))]
         [#x04 (unit `(ed25519verify))]
         [#x05 (>>= read-uint8 (λ (v) (unit `(ecdsa_verify ,v))))]
         [#x06 (>>= read-uint8 (λ (v) (unit `(ecdsa_pk_decompress ,v))))]
         [#x07 (>>= read-uint8 (λ (v) (unit `(ecdsa_pk_recover ,v))))]
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
         [#x20
          (>>= (>>= read-varuint
                    (λ (n)
                      (let loop ([n n])
                        (if (zero? n)
                          (unit (list))
                          (>>= read-varuint
                               (λ (x)
                                 (>>= (loop (sub1 n))
                                      (λ (xs) (unit (cons x xs))))))))))
               (λ (ns) (unit `(intcblock . ,ns))))]
         [#x21 (>>= read-uint8 (λ (i) (unit `(intc ,i))))]
         [#x22 (unit `(intc_0))]
         [#x23 (unit `(intc_1))]
         [#x24 (unit `(intc_2))]
         [#x25 (unit `(intc_3))]
         [#x26
          (>>= (>>= read-varuint
                    (λ (n)
                      (let loop ([n n])
                        (if (zero? n)
                          (unit (list))
                          (>>= read-bytes
                               (λ (bs)
                                 (>>= (loop (sub1 n))
                                      (λ (bss) (unit (cons bs bss))))))))))
               (λ (bss) (unit `(bytecblock . ,bss))))]
         [#x27 (>>= read-uint8 (λ (i) (unit `(bytec ,i))))]
         [#x28 (unit `(bytec_0))]
         [#x29 (unit `(bytec_1))]
         [#x2a (unit `(bytec_2))]
         [#x2b (unit `(bytec_3))]
         [#x31
          (>>= transaction-field (λ (f) (unit `(txn ,f))))]
         [#x32
          (>>= global-field (λ (f) (unit `(global ,f))))]
         [#x34
          (>>= read-uint8 (λ (i) (unit `(load ,i))))]
         [#x35
          (>>= read-uint8 (λ (i) (unit `(store ,i))))]
         [#x36
          (>>= transaction-field
               (λ (f)
                 (>>= read-uint8
                      (λ (ai)
                        (unit `(txna ,f ,ai))))))]
         [#x38
          (>>= transaction-field (λ (f) (unit `(gtxns ,f))))]
         [#x40
          (>>= destination (λ (dst) (unit `(bnz ,dst))))]
         [#x41
          (>>= destination (λ (dst) (unit `(bz ,dst))))]
         [#x42
          (>>= destination (λ (dst) (unit `(b ,dst))))]
         [#x43 (unit `(return))]
         [#x44 (unit `(assert))]
         ; #x45-#x47 unused
         [#x48 (unit `(pop))]
         [#x49 (unit `(dup))]
         [#x4a (unit `(dup2))]
         [#x4b (>>= read-uint8 (λ (n) (unit `(dig ,n))))]
         [#x4c (unit `(swap))]
         [#x4d (unit `(select))]
         [#x4e (>>= read-uint8 (λ (n) (unit `(cover ,n))))]
         [#x4f (>>= read-uint8 (λ (n) (unit `(uncover ,n))))]
         [#x50 (unit `(concat))]
         [#x51 (>>= read-uint8 (λ (s) (>>= read-uint8 (λ (e) (unit `(substring ,s ,e))))))]
         [#x52 (unit `(substring3))]
         [#x62 (unit `(app_local_get))]
         [#x64 (unit `(app_global_get))]
         [#x65 (unit `(app_global_get_ex))]
         [#x66 (unit `(app_local_put))]
         [#x67 (unit `(app_global_put))]
         [#x68 (unit `(app_local_del))]
         [#x69 (unit `(app_global_del))]
         [#x80 (>>= read-bytes (λ (bs) (unit `(pushbytes ,bs))))]
         [#x81 (>>= read-varuint (λ (n) (unit `(pushint ,n))))]
         [#x88
          (>>= destination (λ (dst) (unit `(callsub ,dst))))]
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
         [#xb5 (>>= transaction-field (λ (f) (>>= read-uint8 (λ (i) (unit `(itxna ,f ,i))))))]
         ; #xb6-#xbf unused
         [#xc0 (>>= transaction-field (λ (f) (unit `(txnas ,f))))]
         )))

(define (register-instruction i instr)
  (update 'instructions (λ (h) (hash-set h i instr)) (hasheqv)))

(define disassemble-instruction-stream
  (>>= position
       (λ (lft)
         (∨ (>> stream-end
                (update 'instructions (λ (h) (hash-set h lft `(done))) (hasheqv)))
            (>> (>>= disassemble-instruction
                    (λ (instr)
                      (>>= position
                           (λ (rgt)
                             (update 'instructions (λ (h) (hash-set h lft `(succ ,instr ,rgt))) (hasheqv))))))
                disassemble-instruction-stream)))))

(define disassemble
  (>> (∨ (>>= read-varuint (λ (lsv) (set 'logic-sig-version lsv)))
         (void error 'couldnt "read thing"))
      (>>= position (λ (i) (set 'initial i)))
      disassemble-instruction-stream))

(module+ main
  (require racket/pretty
           racket/port)

  (match (disassemble (port->bytes (current-input-port))
                      (hasheq)
                      0)
    [(list* (list) s i)
     (let ([instructions (hash-ref s 'instructions)]
           [start-i (hash-ref s 'initial)])
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
         (let loop ([i start-i])
           (match (hash-ref instructions i)
             [`(succ ,instr ,next-i)
              (match instr
                [(list (and code (or 'bnz 'bz 'b 'callsub))
                       pc)
                 (cond
                   [(hash-ref phs pc #f)
                    => (λ (ph)
                         (placeholder-set! (hash-ref phs i)
                                           (cons (list code ph)
                                                 (hash-ref phs next-i))))]
                   [else
                    (error 'disassemble "jump to global offset ~a not on instruction boundary" pc)])]
                [(or `(err) `(return))
                 (placeholder-set! (hash-ref phs i) (cons instr (list)))]
                [_
                 (void)])
              (loop next-i)]
             [`(done)
              (void)]))
         (pretty-print
          (make-reader-graph (hash-ref phs start-i)))))]))
