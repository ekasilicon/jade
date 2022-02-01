#lang racket/base
(require (only-in racket/match match match-lambda)
         (only-in racket/set seteqv set-add set->list)
         "static/sumtype.rkt"
         "static/object.rkt"
         "error.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "prefix.rkt"
         "version.rkt"
         "instruction/read.rkt"
         "instruction/control.rkt"
         "assembly.rkt")

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
            [mupd
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
                               (mupd 'instructions (λ (h) (hash-set h lft (list))) (hasheqv)))
                           (>> (mplus (>>= disassemble-instruction
                                           (λ (instr)
                                             (>>= position
                                                  (λ (rgt)
                                                    (mupd 'instructions (λ (h) (hash-set h lft (cons instr rgt))) (hasheqv))))))
                                      (fail/context (λ (fail bs i) (fail "unrecognized instruction at byte offset ~a: ~a" i (number->string (bytes-ref bs i) 16)))))
                               disassemble-instruction-stream-inner))))]
            [disassemble-instruction-stream
             (>> (>>= position (mset 'initial))
                 disassemble-instruction-stream-inner
                 (>>= position (mset 'final)))])))

(define (σ→directives lsv σ)
  (let/ec return
    ; instructions is a map from a left boundary to a (list) or (cons instr next-i)
    ; where instr is the instruction at that boundary and next-i is the right boundary
    ; start-i is the left boundary of the first instruction
    (let ([instructions (hash-ref σ 'instructions)])
      (define-values (offset offset-map)
        (let ([control (fix (instruction-control/version lsv))])
          (values (control 'offset)
                  (control 'offset-map))))
      (let ([dst→ℓ (let ([dsts (let loop ([i (hash-ref σ 'initial)]
                                          [dsts (seteqv)])
                                 (match (hash-ref instructions i)
                                   [(list)
                                    dsts]
                                   [(cons instr next-i)
                                    (loop next-i
                                          (cond
                                            [(offset instr)
                                             => (λ (dst)
                                                  (cond
                                                    [(= dst (hash-ref σ 'final))
                                                     (if (< lsv 2)
                                                       (return (error 'invalid-destination "cannot jump to end of instruction block prior to v2"))
                                                       (set-add dsts dst))]
                                                    [(hash-has-key? instructions dst)
                                                     (set-add dsts dst)]
                                                    [else
                                                     (return (error 'invalid-destination "jump not on instruction boundary"))]))]
                                            [else
                                             dsts]))]))])
                     (for/hasheqv ([i (in-list (sort (set->list dsts) <))]
                                   [j (in-naturals 1)])
                       (values i (string->symbol (format "label~a" j)))))])
        (let loop ([i (hash-ref σ 'initial)])
          (append (cond
                    [(hash-ref dst→ℓ i #f)
                     => (λ (ℓ) (list (label ℓ)))]
                    [else
                     (list)])
                  (match (hash-ref instructions i)
                    [(list)
                     (list)]
                    [(cons instr next-i)
                     (cons (instruction [instruction (offset-map (λ (i) (hash-ref dst→ℓ i)) instr)]) 
                           (loop next-i))])))))))

(define (disassemble bs)
  (match (read-prefix bs)
    [(cons lsv bytecode)
     (if (and (exact-nonnegative-integer? lsv)
              (memv lsv '(1 2 3 4 5 6)))
       (let ([disassemble (fix (mix (instruction-read/version lsv)
                                    (disassemble/version lsv)))])
         (sumtype-case Result ((disassemble 'disassemble-instruction-stream)
                               bytecode 0 (hasheqv))
           [(success [xs (list)] σ)
            (assembly [logic-sig-version lsv]
                      [directives (σ→directives lsv σ)])]
           [(failure message)
            (error 'disassemble-failure message)]))
       (error 'disassemble-bad-header "expected TEAL version 1, 2, 3, 4, 5, or 6 but got ~a" lsv))]
    [#f
     (error 'disassemble-bad-header "expected TEAL version encoded as a varuint")]))

(provide disassemble)

(module+ main
  (require racket/port
           racket/pretty
           "assembly/control.rkt")
  
  (pretty-print
   (control-flow-graph
    (disassemble
     (port->bytes (current-input-port))))))
