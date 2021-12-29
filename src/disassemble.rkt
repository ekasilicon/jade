#lang racket/base
(require racket/match
         racket/set
         racket/port
         #;"static/record.rkt"
         "static/sumtype.rkt"
         "static/object.rkt"
         "monad.rkt"
         "read-byte.rkt"
         (prefix-in i: "instruction.rkt"))

(define-sumtype Result
  (failure message)
  (success values state index))

(define dis0
  (mix monad-extras
       monad+-extras
       read-byte-extras
       (inc ()
            [unit
             (λ xs
               (λ (bs s i)
                 (success [values xs] [state s] [index i])))]
            [>>=
             (λ (m f)
               (λ (bs s i)
                 (match (m bs s i)
                   [(success [values xs] [state s] [index i])
                    ((apply f xs) bs s i)]
                   [(failure message)
                    (failure message)]
                   [#f
                    #f])))]
            [mplus
             (λ ms
               (λ (bs s i)
                 (ormap (λ (m) (m bs s i)) ms)))]
            [read-byte
             (λ (bs s i)
               (if (= (bytes-length bs) i)
                 #f
                 (success [values (list (bytes-ref bs i))]
                          [state s]
                          [index (add1 i)])))]
            [fail
             (λ (template . args)
               (λ (bs s i)
                 (failure [message (apply format template args)])))]
            [fail/context
             (λ (make)
               (λ (bs s i)
                 (make (λ (template . args) (failure [message (apply format template args)])) bs i)))]
            [stream-end
             (λ (bs s i)
               (if (= (bytes-length bs) i)
                 (success [values (list)] [state s] [index i])
                 #f))]
            [position
             (λ (bs s i)
               (success [values (list i)] [state s] [index i]))]
            [mget
             (λ (k d)
               (λ (bs s i)
                 (success [values (list (hash-ref s k d))] [state s] [index i])))]
            [mset
             (λ (k d)
               (λ (bs s i)
                 (success [values (list)] [state (hash-set s k v)] [index i])))]
            [mupdate
             (λ (k d)
               (λ (bs s i)
                 (success [values (list)] [state (hash-update s k f d)] [index i])))]
            [disassemble-instruction
             (>>= read-instruction process-instruction)]
            [offset→destination
             (>>= position (λ (pc) (unit (+ pc offset))))])))

(define dis1
  (inc (offset→destination
        unit >>=)
       [process-instruction
        (sumtype-case-lambda i:Instruction1
          [(i:bnz offset)
           (>>= (offset→destination offset)
                (λ (dst) (unit (i:bnz [offset dst]))))]
               #:otherwise unit)]))

(define dis2
  (inc (unit >>=)
       [process-instruction
        (sumtype-case-lambda i:Instruction2
          [(i:Instruction1 instr)
           ((super process-instruction) instr)]
          [(i:b offset)
           (>>= (offset→destination offset)
                (λ (dst) (unit (i:b [offset dst]))))]
          [(i:bz offset)
           (>>= (offset→destination offset)
                (λ (dst) (unit (i:bz [offset dst]))))]
          #:otherwise unit)]))

(define dis3
  (inc (unit)
       [process-instruction
        (sumtype-case-lambda i:Instruction3
          [(i:Instruction2 instr)
           ((super process-instruction) instr)]
          #:otherwise unit)]))

(define dis4
  (inc (unit >>=)
       [process-instruction
        (sumtype-case-lambda i:Instruction4
          [(i:Instruction3 instr)
           ((super process-instruction) instr)]
          [(i:callsub offset)
           (>>= (offset→destination offset)
                (λ (dst) (unit (i:callsub [offset dst]))))]
          #:otherwise unit)]))

(define dis5
  (inc (unit)
       [process-instruction
        (sumtype-case-lambda i:Instruction5
          [(i:Instruction4 instr)
           ((super process-instruction) instr)]
          #:otherwise unit)]))

(define dis6
  (inc (unit)
       [process-instruction
        (sumtype-case-lambda i:Instruction5
          [(i:Instruction4 instr)
           ((super process-instruction) instr)]
          #:otherwise unit)]))

(require "version.rkt")

(define disassemble/version
  (make-*/version 'disassemble/version dis0 dis1 dis2 dis3 dis4 dis5 dis6 disassemble-extras))

#;
(define disassemble-instruction
  (>>= (i:read-instruction rb)
       (λ (instr)
         (define (offset→destination offset)
           )
         (sumtype-case i:Instruction instr
           [(i:b offset)
            (>>= (offset→destination offset)
                 (λ (dst) (unit (i:b [offset dst]))))]
           [(i:bz offset)
            (>>= (offset→destination offset)
                 (λ (dst) (unit (i:bz [offset dst]))))]
           [(i:bnz offset)
            (>>= (offset→destination offset)
                 (λ (dst) (unit (i:bnz [offset dst]))))]
           [(i:callsub offset)
            (>>= (offset→destination offset)
                 (λ (dst) (unit (i:callsub [offset dst]))))]
           #:otherwise unit))))


#;
(define disassemble-instruction-stream
  (>>= position
       (λ (lft)
         (mplus (>> stream-end
                    (mupdate 'instructions (λ (h) (hash-set h lft `(done))) (hasheqv)))
                (>> (mplus (>>= disassemble-instruction
                                (λ (instr)
                                  (>>= position
                                       (λ (rgt)
                                         (mupdate 'instructions (λ (h) (hash-set h lft `(succ ,instr ,rgt))) (hasheqv))))))
                           (fail/context (λ (fail bs i) (fail "unrecognized instruction at byte offset ~a: ~a" i (number->string (bytes-ref bs i) 16)))))
                    disassemble-instruction-stream)))))

#;
(define disassemble
  (>> (mplus (>>= (read-varuint rb) (mset 'logic-sig-version))
             (fail "unable to read initial logic signature version sequence"))
      (>>= position (mset 'initial))
      disassemble-instruction-stream))

#;
(define (run bs)
  (disassemble bs (hasheq) 0))

#;
(define (disassemble-bytes bs)
  (match (run bs)
    [(success [values (list)] state index)
     (state→assembly state)]
    [(failure message)
     (error 'disassemble message)]
    [#f
     (error 'disassemble "internal error")]))

#;
(define (disassemble-port ip)
  (disassemble-bytes (port->bytes ip)))

#;
(define-sumtype Directive
  i:Instruction
  (pragma content)
  (label ℓ))

#;
(provide (sumtype-out Directive))

#;
(define instruction-line
  (sumtype-case-lambda i:Instruction
    #:otherwise (λ (_) "  instruction")))

#;
(define directive-line
  (sumtype-case-lambda Directive
    [(i:Instruction instr)
     (instruction-line instr)]
    [(pragma content)
     (format "#pragma ~a" content)]
    [(label ℓ)
     (format "~a:" ℓ)]))

#;
(provide directive-line)

#;
(define-syntax instruction-parser
  (syntax-parser
    [_
     (match-let ([(sumtype-info variants) (syntax-local-value #'Directive)])
       (with-syntax ([(parser ...)
                      (map
                       (λ (variant)
                         (match-let ([(record-info fields _ constructor _ _ _) (syntax-local-value variant)])
                           (with-syntax ([name (symbol->string (syntax->datum variant))]
                                         [constructor constructor]
                                         [(field ...) (map
                                                        (λ (field)
                                                          (match field
                                                            ['v #'guarded-uint8]
                                                            ['i #'guarded-uint8]
                                                            ['n #'guarded-uint8]
                                                            ['uints #'(p* (>> whitespace* varuint))]
                                                            ['bytess #'(p* (>> whitespace* pbytes))]
                                                            ['group-index #'guarded-uint8]
                                                            ['array-index #'guarded-uint8]
                                                            ['offset #'guarded-label]
                                                            ['start #'guarded-uint8]
                                                            ['end #'guarded-uint8]
                                                            ['length #'guarded-uint8]
                                                            ['bytes #'guarded-pbytes]
                                                            ['uint #'guarded-varuint]
                                                            ['field
                                                             (match (syntax->datum variant)
                                                               [(or 'txn 'gtxn 'txna 'gtxna 'gtxns 'gtxnsa 'itxn_field 'itxn 'itxna 'txnas 'gtxnas 'gtxnsas)
                                                                #'transaction-field]
                                                               ['global #'global-field]
                                                               ['asset_holding_get #'asset-holding-field]
                                                               ['asset_params_get #'asset-params-field]
                                                               ['app_params_get #'app-params-field])]
                                                            ))
                                                        fields)])
                             #'(make-instruction name constructor field ...))))
                       variants)])
         #'(∨ parser ...)))]))

(define (state→assembly offset σ)
  (cons (pragma [content (format "version ~a" (hash-ref σ 'logic-sig-version))])
        (let ([instrs (hash-ref σ 'instructions)]
              [start-i (hash-ref σ 'initial)])
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
                                  (cond
                                    [((offset 'offset) instr)
                                     => (λ (dst)
                                          (if (hash-has-key? instrs dst)
                                            (set-add dsts dst)
                                            (error 'disassemble "branch to byte offset ~a not on instruction boundary" dst)))]
                                    [else
                                     dsts]))]))]
                 [dsts (for/hash ([dst (in-list (sort (set->list dsts) <))]
                                  [i (in-naturals)])
                         (values dst (string->symbol (format "label~a" i))))])
            (let loop ([i start-i])
              (let ([instrs (match (instr-ref i)
                              [`(done)
                               (list)]
                              [`(succ ,instr ,next-i)
                               (cons ((offset 'offset-map) (λ (dst) (hash-ref dsts dst)) instr)
                                     (loop next-i))])])
                (cond
                  [(hash-ref dsts i #f)
                   => (λ (ℓ) (cons (label ℓ) instrs))]
                  [else instrs])))))))



(apply fix (take (reverse instruction-offsets) lsv))

(define (σ→AST σ)
  (let ([instructions (hash-ref σ 'instructions)]
        [start-i (hash-ref σ 'initial)])
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
           (define (resolve dst make)
             (cond
               [(hash-ref phs dst #f)
                => (λ (ph)
                     (placeholder-set! (hash-ref phs i) (cons (make ph) (hash-ref phs next-i))))]
               [else
                (error 'disassemble "branch to byte offset ~a not on instruction boundary" dst)]))
           (define (terminal instr)
             (placeholder-set! (hash-ref phs i)
                               (cons instr (list))))
           (let* ([instr ((offset 'offset-map) )]))
           (cond
             [])
           (sumtype-case i:Instruction instr
             [(i:bz [offset dst])
              (resolve dst (λ (ph) (i:bz [offset ph])))]
             [(i:bnz [offset dst])
              (resolve dst (λ (ph) (i:bnz [offset ph])))]
             [(i:callsub [offset dst])
              (resolve dst (λ (ph) (i:callsub [offset ph])))]
             [(i:b [offset dst])
              ; don't set it to the contents of the destination placeholder
              ; because those might be changed by this loop
              (placeholder-set! (hash-ref phs i)
                                (list (i:b [offset (hash-ref phs dst)])))]
             [(i:err)
              (terminal instr)]
             [(i:retsub)
              (terminal instr)]
             [(i:return)
              (terminal instr)]
             #:otherwise void)
           (loop next-i)]
          [`(done)
           (void)]))
      (make-reader-graph (hash-ref phs start-i)))))

#;
(provide disassemble-bytes
         disassemble-port)

#;
(module+ main
  (require racket/pretty)

  (pretty-print (disassemble-port (current-input-port))))
