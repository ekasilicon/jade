#lang racket/base
(require racket/match
         "static/sumtype.rkt"
         "static/object.rkt"
         "monad.rkt"
         "read-byte.rkt"
         "prefix.rkt"
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
                 disassemble-instruction-stream-inner)])))


(define (σ→AST lsv σ)
  (let ([instructions (hash-ref σ 'instructions)]
        [start-i (hash-ref σ 'initial)])
    ; instructions is a map from a left boundary to a `(succ ,instr ,next-i)
    ; where instr is the instruction at that boundary and next-i is the right boundary
    ; start-i is the left boundary of the first instruction
    (let ([start-ph (make-placeholder #f)])
      (resolve-CFG-placeholders
       lsv
       ; this loop does a pass through the instructions
       ; to create a map from left boundaries to placeholders
       ; which contain the instruction sequence (itself indirected by internal placeholders)
       (let loop ([i start-i]
                  [phs (hash-set (hasheqv) start-i start-ph)])
         (match (hash-ref instructions i)
           [(list)
            (placeholder-set! (hash-ref phs i) (list))
            phs]
           [(cons instr next-i)
            (let ([ph (make-placeholder #f)])
              (placeholder-set! (hash-ref phs i) (cons instr ph))
              (loop next-i (hash-set phs next-i ph)))]))
       start-ph)
      (make-reader-graph start-ph))))

(define (disassemble bs)
  (match (read-prefix bs)
    [(cons lsv bytecode)
     (let ([disassemble (fix (mix (instruction-read/version lsv)
                                  (disassemble/version lsv)))])
       (sumtype-case Result ((disassemble 'disassemble-instruction-stream)
                             bytecode 0 (hasheqv))
         [(success [xs (list)] σ)
          (cons lsv (σ→AST lsv σ))]
         [(failure message)
          (error 'disassemble message)]))]
    [#f
     (error 'disassemble "expected a logic signature version encoded as a varuint")]))

(provide disassemble)
