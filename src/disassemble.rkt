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
          [26 'ApplicationArgs]
          [27 'NumAppArgs]))))

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
         [#x08 (unit `(+))]
         [#x0a (unit `(/))]
         [#x0d (unit '(>))]
         [#x01 (unit `(sha256))]
         [#x12 (unit `(==))]
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
         [#x22 (unit `(intc_0))]
         [#x23 (unit `(intc_1))]
         [#x24 (unit `(intc_2))]
         [#x31
          (>>= transaction-field (λ (f) (unit `(txn ,f))))]
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
         [#x40
          (>>= destination (λ (dst) (unit `(bnz ,dst))))]
         [#x42
          (>>= destination (λ (dst) (unit `(b ,dst))))]
         [#x43 (unit `(return))]
         [#x50 (unit `(concat))]
         [#x52 (unit `(substring3))]
         [#x65 (unit `(app_global_get_ex))]
         [#x67 (unit `(app_global_put))]
         [#x80
          (>>= read-bytes (λ (bs) (unit `(pushbytes ,bs))))]
         [#x88
          (>>= destination (λ (dst) (unit `(callsub ,dst))))]
         [#x89 (unit `(retsub))])))

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
                [_
                 (void)])
              (loop next-i)]
             [`(done)
              (void)]))
         (pretty-print
          (make-reader-graph (hash-ref phs start-i))))
       #;
       (let loop ([i (hash-ref s 'initial)]
                  [phs (hasheqv)])
         (match (hash-ref instructions i)
           [`(succ ,instr ,next-i)
            (let* ([phs (if (hash-has-key? phs i)
                          phs
                          (hash-set phs i (make-placeholder #f)))]
                   [ph (hash-ref phs i)])
              (placeholder-set! i (cons )))
            (let ([ph (hash-ref phs i (make-placeholder #f))])
              )
            (let ([ph (make-placeholder #f)]))
            (placeholder-set! (hash-ref resolved i) `(cons))]
           )
         )
       #;
       (if (andmap (λ (dst) (hash-has-key? instructions dst)) (hash-ref s 'destinations))
         (pretty-print
          (let loop ([i (hash-ref s 'initial)]
                    [resolved (hasheqv)])
           (if (hash-has-key? resolved i)
             resolved
             (match (hash-ref instructions i)
               [`(succ ,instr ,new-i)
                (let ([resolved (loop new-i (hash-set resolved i #f))])
                  (hash-set resolved i (cons instr (hash-ref resolved new-i))))]
               [`(done)
                (hash-set resolved i (list))]))
           ))
         
         (error 'disassemble "jump target not on instruction boundary")))
     #;
     (pretty-print
      
      (foldl
       (λ ())
       (hasheqv)
       ))
     ]))
