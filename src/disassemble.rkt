#lang racket/base
(require racket/match
         "static/sumtype.rkt"
         "static/object.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "version.rkt"
         "instruction/read.rkt"
         "instruction/control.rkt")

(define-sumtype Result
  (failure message)
  (success xs i σ))

(define (disassemble/version lsv)
  (mix (instruction-control/version lsv)
       read-byte-extras
       monad+-extras
       monad-extras
       (inc (>>
             read-instruction
             offset-map)
            [unit
             (λ xs (λ (bs i σ) (success xs i σ)))]
            [>>=
             (λ (m f)
               (λ (bs i σ)
                 (match (m bs i σ)
                   [(success xs i σ)
                    ((apply f xs) bs i σ)]
                   [(failure message)
                    (failure message)]
                   [#f
                    #f])))]
            [mplus
             (λ ms (λ (bs i σ) (ormap (λ (m) (m bs i σ)) ms)))]
            [read-byte
             (λ (bs i σ)
               (if (= (bytes-length bs) i)
                 #f
                 (success [xs (list (bytes-ref bs i))] [i (add1 i)] σ)))]
            #;
            [fail
             (λ (template . args)
               (λ (bs i σ)
                 (failure [message (apply format template args)])))]
            [fail/context
             (λ (make)
               (λ (bs i σ)
                 (make (λ (template . args) (failure [message (apply format template args)])) bs i)))]            
            [mget
             (λ (k d)
               (λ (bs i σ)
                 (success [xs (list (hash-ref σ k d))] i σ)))]
            [mset
             (λ (k)
               (λ (v)
                 (λ (bs i σ)
                   (success [xs (list)] i [σ (hash-set σ k v)]))))]
            [mupdate
             (λ (k f d)
               (λ (bs i σ)
                 (success [xs (list)] i [σ (hash-update σ k f d)])))]
            [disassemble-instruction
             (>>= read-instruction
                  (λ (instr)
                    (>>= position
                         (λ (pc)
                           (unit (offset-map (λ (offset) (+ pc offset)) instr))))))]
            [stream-end
             (λ (bs i σ)
               (if (= (bytes-length bs) i)
                 (success [xs (list)] i σ)
                 #f))]
            [position
             (λ (bs i σ) (success [xs (list i)] i σ))]
            [disassemble-instruction-stream-inner
             (>>= position
                  (λ (lft)
                    (mplus (>> stream-end
                               (mupdate 'instructions (λ (h) (hash-set h lft (list))) (hasheqv)))
                           (>> (mplus (>>= disassemble-instruction
                                           (λ (instr)
                                             (>>= position
                                                  (λ (rgt)
                                                    (mupdate 'instructions (λ (h) (hash-set h lft (cons instr rgt))) (hasheqv))))))
                                      (fail/context (λ (fail bs i) (fail "unrecognized instruction at byte offset ~a: ~a" i (number->string (bytes-ref bs i) 16)))))
                               disassemble-instruction-stream-inner))))]
            [disassemble-instruction-stream
             (>> (>>= position (mset 'initial))
                 disassemble-instruction-stream-inner)])))


(define (σ→AST lsv σ)
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
                   [(list)
                    (placeholder-set! (hash-ref phs i) (list))
                    phs]
                   [(cons instr next-i)
                    (let ([ph (make-placeholder #f)])
                      (placeholder-set! (hash-ref phs i) (cons instr ph))
                      (loop next-i (hash-set phs next-i ph)))]))])
      (resolve-CFG-placeholders lsv phs (hash-ref phs start-i))
      #;
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


(define (disassemble bs)
  (cond
    [(((fix (mix read-byte-extras
                 monad-extras
                 (inc ()
                      [unit
                        (λ xs (λ (bs i) (cons xs i)))]
                      [>>=
                       (λ (m f)
                         (λ (bs i)
                           (cond
                             [(m bs i)
                              => (match-lambda
                                   [(cons xs i)
                                    ((apply f xs) bs i)])]
                             [else
                              #f])))]
                      [read-byte
                       (λ (bs i)
                         (if (= i (bytes-length bs))
                           #f
                           (cons (list (bytes-ref bs i)) (add1 i))))])))
       'read-varuint)
      bs 0)
     => (match-lambda
          [(cons (list lsv) i)
           (let ([disassemble (fix (mix (instruction-read/version lsv)
                                        (disassemble/version lsv)))])
             (sumtype-case Result ((disassemble 'disassemble-instruction-stream)
                                   bs i (hasheqv))
               [(success [xs (list)] σ)
                (cons lsv (σ→AST lsv σ))]
               [(failure message)
                (error 'disassemble message)]))])]
    [else
     (error 'disassemble "expected a logic signature version encoded as a varuint")]))

(provide disassemble)


#|



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


#;
(provide disassemble-bytes
         disassemble-port)

#;
(module+ main
  (require racket/pretty)

  (pretty-print (disassemble-port (current-input-port))))
|#
