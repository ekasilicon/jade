#lang racket/base
(require (only-in racket/match match match-lambda)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../instruction/control.rkt"
         "../assembly.rkt")

(define (assembly-control/version lsv)
  (mix (inc ()
            [offset-map
             (λ (f instr)
               (sumtype-case Pseudoinstruction instr
                 [(varuint-immediate value)
                  (varuint-immediate value)]
                 [(bytes-immediate value)
                  (bytes-immediate value)]
                 [(instruction [instruction instr])
                  (instruction [instruction ((super 'offset-map) f instr)])]))]
            [has-inorder-successor?
             (sumtype-case-lambda Pseudoinstruction
               [(varuint-immediate)
                #t]
               [(bytes-immediate)
                #t]
               [(instruction [instruction instr])
                ((super 'has-inorder-successor?) instr)])])
       (instruction-control/version lsv)))

(provide assembly-control/version)

(define (control-flow-graph asm) 
  (match asm
    [(assembly [logic-sig-version lsv] directives)
     (let* ([initial-ph (make-placeholder #f)]
            [phs (let loop ([directives directives]
                            [ph initial-ph]
                            [phs (hasheq)])
                   (match directives
                     [(list)
                      (placeholder-set! ph (list))
                      phs]
                     [(cons directive directives)
                      (sumtype-case Directive directive
                        [(pragma)
                         (loop directives ph phs)]
                        [(label ℓ)
                         (loop directives ph (hash-set phs ℓ ph))]
                        [(Pseudoinstruction instr)
                         (let ([next-ph (make-placeholder #f)])
                           (placeholder-set! ph (cons instr next-ph))
                           (loop directives next-ph phs))])]))])
       (define-values (offset-map has-inorder-successor?)
         (let ([o (fix (assembly-control/version lsv))])
           (values (o 'offset-map)
                   (o 'has-inorder-successor?))))
       ; this loop does a pass to fix instructions whose successor
       ; isn't simply the next instruction
       ; these instructions include terminal instructions,
       ; such as err, return, and retsub,
       ; and branching instructions,
       ; such as bnz, bz, and callsub.
       (let/ec return
         (let loop ([ph initial-ph])
           (match (placeholder-get ph)
             [(list)
              (void)]
             [(cons instr next-ph)
              (let ([instr (offset-map
                            (λ (ℓ)
                              (cond
                                [(hash-ref phs ℓ #f)
                                 => values]
                                [else
                                 (return (error 'invalid-destination "invalid destination label ~a" ℓ))]))
                            instr)])
                (placeholder-set!
                 ph
                 (cons instr
                       ; instructions such as return, b, and err
                       ; never proceed to the next instruction
                       (if (has-inorder-successor? instr)
                         next-ph
                         (list)))))
              (loop next-ph)]))
         (cons lsv
               (make-reader-graph initial-ph))))]))

(provide control-flow-graph)
