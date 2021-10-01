#lang racket/base
(require racket/match
         racket/set
         racket/port
         "record.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "instruction.rkt")

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

(define destination
  (>>= (read-int16 rb) 
       (λ (offset)
         (>>= position
              (λ (i)
                (let ([dst (+ i offset)])
                  (>> (push-destination dst)
                      (unit dst))))))))

(define disassemble-instruction
  (>>= (read-instruction rb)
       (match-lambda
         [(list (and code (or 'b 'bz 'bnz 'callsub))
                offset)
          (>>= position
               (λ (i)
                 (let ([dst (+ i offset)])
                   (>> (push-destination dst)
                       (unit (list code dst))))))]
         [instr
          (unit instr)])))

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
